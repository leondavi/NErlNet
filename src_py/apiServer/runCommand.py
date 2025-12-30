import os
import subprocess, pathlib

def force_kill(process):
    if process.returncode is None:
        os.system("pkill -TERM -P %s"%process.pid)

class RunCommand():
    def __init__(self, cmd : str, cwd = pathlib.Path.home()) -> None:
        self.cwd = cwd
        self.cmd = cmd
        self.process = subprocess.Popen("exec "+self.cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=self.cwd)
        self.pid = self.process.pid

    TIMEOUT_PROC_KILLED=-1
    def sync(self, timeout_sec):
        try:
            stdout, stderr = self.process.communicate(timeout=timeout_sec)
            exit_code = self.process.returncode
            stdout_str = stdout.decode("utf-8")
            stderr_str = stderr.decode("utf-8")
            return stdout_str, stderr_str, exit_code
        except subprocess.TimeoutExpired:
            return None, None, self.TIMEOUT_PROC_KILLED
        except Exception:
            return None, None, self.TIMEOUT_PROC_KILLED

    def __format__(self, __format_spec: str) -> str:
        return f'{self.cmd} rc: {self.process.returncode}'
