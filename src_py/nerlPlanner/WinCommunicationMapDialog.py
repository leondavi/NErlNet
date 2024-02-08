import PySimpleGUI as sg

COMMUNICATION_MAP_WINDOW_TITLE = "Generate Communication Map Json File"

def WinCommunicationMapDialog():
    CommunicationMapWindow  = sg.Window(title=COMMUNICATION_MAP_WINDOW_TITLE, layout=[[]],modal=True, keep_on_top=True)                                                  
    while True:
        event, values = CommunicationMapWindow.read()

        if event == "Exit" or event == sg.WIN_CLOSED:
            break
    CommunicationMapWindow.close()