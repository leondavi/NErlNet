import os from 'node:os';
import { exec } from 'node:child_process';
import { promisify } from 'node:util';

const execAsync = promisify(exec);
const MAX_HOSTS = 256;
const CONCURRENCY = 32;

type NetworkInterfaceInfo = {
  address: string;
  netmask: string;
};

const isValidIPv4 = (ip: string): boolean => {
  const pattern = /^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$/;
  return pattern.test(ip);
};

const ipToInt = (ip: string) =>
  ip
    .split('.')
    .map((octet) => Number(octet))
    .reduce((acc, octet) => (acc << 8) + octet, 0) >>> 0;

const intToIp = (value: number) =>
  [24, 16, 8, 0].map((shift) => (value >>> shift) & 255).join('.');

const netmaskToPrefix = (mask: string) =>
  mask
    .split('.')
    .map((octet) => Number(octet))
    .reduce((count, octet) => {
      let bits = 0;
      let value = octet;
      while (value) {
        bits += value & 1;
        value >>= 1;
      }
      return count + bits;
    }, 0);

const getInterfaces = (): NetworkInterfaceInfo[] => {
  const interfaces = os.networkInterfaces();
  const results: NetworkInterfaceInfo[] = [];
  Object.values(interfaces).forEach((entries) => {
    entries?.forEach((entry) => {
      if (entry.family === 'IPv4' && !entry.internal) {
        results.push({ address: entry.address, netmask: entry.netmask });
      }
    });
  });
  return results;
};

const buildRange = (address: string, netmask: string): string[] => {
  const prefix = netmaskToPrefix(netmask);
  const baseIp = ipToInt(address);
  const usedPrefix = prefix < 24 ? 24 : prefix;
  const size = Math.min(2 ** (32 - usedPrefix), MAX_HOSTS);
  const mask = size === 0 ? 0 : ~(size - 1);
  const start = baseIp & mask;
  const end = start + size - 1;
  const ips: string[] = [];
  for (let current = start + 1; current <= end - 1; current += 1) {
    ips.push(intToIp(current));
  }
  return ips.slice(0, MAX_HOSTS);
};

const commandExists = async (command: string) => {
  try {
    await execAsync(`command -v ${command}`);
    return true;
  } catch {
    return false;
  }
};

const pingHost = async (ip: string) => {
  if (!isValidIPv4(ip)) {
    return false;
  }
  const platform = process.platform;
  const command =
    platform === 'win32'
      ? `ping -n 1 -w 1000 ${ip}`
      : platform === 'darwin'
      ? `ping -c 1 -W 1000 ${ip}`
      : `ping -c 1 -W 1 ${ip}`;
  try {
    await execAsync(command);
    return true;
  } catch {
    return false;
  }
};

const runPool = async <T>(
  items: T[],
  limit: number,
  worker: (item: T) => Promise<void>
) => {
  let index = 0;
  const runners = Array.from({ length: limit }).map(async () => {
    while (index < items.length) {
      const current = items[index];
      index += 1;
      await worker(current);
    }
  });
  await Promise.all(runners);
};

const scanWithPing = async (ips: string[]) => {
  const reachable: string[] = [];
  await runPool(ips, CONCURRENCY, async (ip) => {
    if (await pingHost(ip)) {
      reachable.push(ip);
    }
  });
  return reachable;
};

const isValidCIDR = (cidr: string): boolean => {
  const pattern = /^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\/(?:[12]?[0-9]|3[0-2])$/;
  return pattern.test(cidr);
};

const scanWithNmap = async (cidrRanges: string[]) => {
  const reachable = new Set<string>();
  for (const cidr of cidrRanges) {
    if (!isValidCIDR(cidr)) {
      continue;
    }
    try {
      const { stdout } = await execAsync(`nmap -sn ${cidr}`);
      stdout
        .split('\n')
        .filter((line) => line.includes('Nmap scan report for'))
        .forEach((line) => {
          const match = line.match(/Nmap scan report for (.*)$/);
          if (!match) {
            return;
          }
          const host = match[1];
          const ipMatch = host.match(/\(([^)]+)\)/);
          reachable.add(ipMatch ? ipMatch[1] : host.trim());
        });
    } catch {
      return Array.from(reachable);
    }
  }
  return Array.from(reachable);
};

export const scanNetwork = async () => {
  const interfaces = getInterfaces();
  const ranges = interfaces.flatMap((entry) => buildRange(entry.address, entry.netmask));
  const cidrs = interfaces.map((entry) => {
    const prefix = Math.max(netmaskToPrefix(entry.netmask), 24);
    const mask =
      prefix === 32 ? -1 : ~((1 << (32 - prefix)) - 1);
    const base = ipToInt(entry.address) & mask;
    return `${intToIp(base)}/${prefix}`;
  });

  const hasNmap = await commandExists('nmap');
  const reachable = hasNmap
    ? await scanWithNmap(cidrs)
    : await scanWithPing(Array.from(new Set(ranges)));

  const unique = new Set<string>(reachable);
  interfaces.forEach((entry) => unique.add(entry.address));

  return {
    ips: Array.from(unique).sort()
  };
};
