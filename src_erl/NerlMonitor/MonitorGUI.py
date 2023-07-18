from pyrlang.node import Node
import PySimpleGUI as sg

layout = [
            [
                sg.Text("NerlNet Monitor" , key='-TEXT-' , size=(30,1) ,text_color='Black' , font=('SFPro' , 20) , background_color='#078FCA' , justification='center' , pad=(0,0)) ,
                sg.Image('NerlnetLogo.png' , size=(50,50))
            ] ,
            [   sg.Button("NerlNet Graph" , button_color=('#94D5F2' , '#000000') , font=('SFPro' , 12) , size=(10,5)) , 
                sg.Frame('Event Log:' , 
                          layout=[[sg.Column(
                                    [[sg.Multiline('', size=(100, 20), key='-LOG-', autoscroll=True)]])
                                  ]] , 
                          background_color=('#078FCA') , font=('SFPro' , 20) , size=(400,200) , title_color='Black' , vertical_alignment='bottom')
            ] ,
            [
                sg.Button("Close" , button_color=('#94D5F2' , '#000000') , font=('SFPro' , 12) , size=(5,2))
            ]
                
          ]

MainWindow = sg.Window("NErlNet" , layout , margins=(5,5) , size=(640,400) , background_color='#078FCA' , finalize=True , resizable=True , element_justification='c')

while True:
    event , values = MainWindow.read()
    if event == "Close" or event == sg.WIN_CLOSED:
        break

MainWindow.close()