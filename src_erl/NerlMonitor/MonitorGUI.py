from pyrlang.node import Node
import PySimpleGUI as sg

layout = [
            [sg.Text("NerlNet Monitor" , size=(30,1) ,text_color='Black' , font=('SFPro' , 20) , background_color='#A9D8E6' , justification='center' , pad=((0,0) , (10,0)))] ,
            [sg.Button("Close" , button_color=('#94D5F2' , '#000000') , font=('SFPro' , 12))]
          ]

MainWindow = sg.Window("NErlNet" , layout , margins=(10,10) , size=(640,400) , background_color='#A9D8E6' , finalize=True , resizable=True)

while True:
    event , values = MainWindow.read()
    if event == "Close" or event == sg.WIN_CLOSED:
        break

MainWindow.close()