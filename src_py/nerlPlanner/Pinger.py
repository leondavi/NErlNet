# Import modules
import subprocess
import ipaddress
import PySimpleGUI as sg
from Definitions import *
from JsonElements import *

BAR_MAX = 100
KEY_DEV_SCAN_BAR = '-KEY-DEV-SCAN-BAR-'

def pinger(net_addr, devices_online_list, ScannerWindow, i):
    devices_online_list = []
    # Prompt the user to input a network address
    net_addr = '192.168.0.0/24'
    # Create the network
    ip_net = ipaddress.ip_network(net_addr)

    # Get all hosts on that network
    all_hosts = list(ip_net.hosts())


    # For each IP address in the subnet, 
    # run the ping command with subprocess.popen interface
    for i in range(len(all_hosts)):
        output = subprocess.Popen(['ping', '-c','1','-W','0.5', str(all_hosts[i])], stdout=subprocess.PIPE).communicate()[0]
        
        if "Destination host unreachable" in output.decode('utf-8'):
            pass
            #print(str(all_hosts[i]), "is Offline")
        elif "Request timed out" in output.decode('utf-8'):
            pass
            #print(str(all_hosts[i]), "is Offline")
        elif "0 received, 100% packet loss" in output.decode('utf-8'):
            pass
            #print(str(all_hosts[i]), "is Offline")
        else:
            #print(str(all_hosts[i]), "is Online")
            devices_online_list.append(str(all_hosts[i]))
        ScannerWindow[KEY_DEV_SCAN_BAR].update((i+1)/len(all_hosts))
    return 

devices_input_lan_mask = ''
def online_scanner_handler(window, event, values, devices_online_hosts_list):
    global devices_input_lan_mask

    if event == KEY_DEVICES_SCANNER_INPUT_LAN_MASK:
        devices_input_lan_mask = values[KEY_DEVICES_SCANNER_INPUT_LAN_MASK]
    
    if event == KEY_DEVICES_SCANNER_BUTTON and '/' in devices_input_lan_mask:
        dev_ip = Ipv4(devices_input_lan_mask.split('/')[0])
        if dev_ip.error():
            sg.popup_ok(f"input lan ip issue!", keep_on_top=True, title="Wrong Input")
        online_devices_scanner_dialog(devices_input_lan_mask, devices_online_hosts_list)
        
        if devices_online_hosts_list:
            window[KEY_DEVICES_ONLINE_LIST_COMBO_BOX].update(value = devices_online_hosts_list[0], values = devices_online_hosts_list)
        else:
            window[KEY_DEVICES_ONLINE_LIST_COMBO_BOX].update(value = 'not found')
    
    if event == KEY_DEVICES_ONLINE_LIST_COMBO_BOX:
        window[KEY_DEVICES_IP_INPUT].update(values[KEY_DEVICES_ONLINE_LIST_COMBO_BOX])


def online_devices_scanner_dialog(net_lan : str, devices_online_list : list):
    host_idx = 0
    # layout the Window
    scanner_layout = [[sg.Text('A custom progress meter')],
                    [sg.ProgressBar(BAR_MAX, orientation='h', size=(20,20), key=KEY_DEV_SCAN_BAR)],
                    [sg.Cancel()]]
    
    ScannerWindow = sg.Window(title="Worker", layout=[scanner_layout],modal=True, keep_on_top=True)                                                  
    ip_net = None

    try:
        # Create the network
        ip_net = ipaddress.ip_network(net_lan)
    except Exception:
        sg.popup_ok(f"given ip error", keep_on_top=True, title="Wrong Input")
        ScannerWindow.close()
        return

    # Get all hosts on that network
    all_hosts = list(ip_net.hosts())

    while True:
        event, values = ScannerWindow.read(timeout=10)
        if event == 'Cancel' or event == sg.WIN_CLOSED:
            break

        if host_idx < len(all_hosts):
            output = subprocess.Popen(['ping', '-c','1','-W','0.5', str(all_hosts[host_idx])], stdout=subprocess.PIPE).communicate()[0]
            
            if "Destination host unreachable" in output.decode('utf-8'):
                pass
                #print(str(all_hosts[i]), "is Offline")
            elif "Request timed out" in output.decode('utf-8'):
                pass
                #print(str(all_hosts[i]), "is Offline")
            elif "0 received, 100% packet loss" in output.decode('utf-8'):
                pass
                #print(str(all_hosts[i]), "is Offline")
            else:
                #print(str(all_hosts[i]), "is Online")
                devices_online_list.append(str(all_hosts[host_idx]))
            ScannerWindow[KEY_DEV_SCAN_BAR].update(100*(host_idx+1)/len(all_hosts))
            host_idx += 1
        else:
            break

    devices_online_list = list(set(devices_online_list))
    ScannerWindow.close()