import PySimpleGUI as sg

def WinWorkerDialog():
    WorkerWindow  = sg.Window(title="Worker", layout=[[sg.Text(f'Worker Configuration Window:')]],modal=True, keep_on_top=True)                                                  
    choice = None
    while True:
        event, values = WorkerWindow.read()
        if event == "Exit" or event == sg.WIN_CLOSED:
            break
        
    WorkerWindow.close()