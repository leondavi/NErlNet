import os
import subprocess, pathlib
from threading import Timer

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
        timer = Timer(timeout_sec, force_kill, [self.process])
        try:
            timer.start()
            stdout, stderr = self.process.communicate()
            exit_code = self.process.returncode
            stdout_str = stdout.decode("utf-8")
            stderr_str = stderr.decode("utf-8")
            return stdout_str, stderr_str, exit_code
        except Exception:
            return None, None, self.TIMEOUT_PROC_KILLED
        finally:
            timer.cancel()

    def __format__(self, __format_spec: str) -> str:
        return f'{self.cmd} rc: {self.process.returncode}'
