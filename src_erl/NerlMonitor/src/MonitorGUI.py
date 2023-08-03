from term import Atom
from pyrlang.node import Node
from pyrlang.process import Process
import PySimpleGUI as sg
import multiprocessing
from time import sleep
import networkx as nx
import matplotlib.pyplot as plt
from datetime import datetime
import os
import math

def draw_gradient(canvas, start_color, end_color):
    for y in range(0, 200):  # Adjust the range to your desired height
        r = start_color[0] + (end_color[0] - start_color[0]) * y / 200
        g = start_color[1] + (end_color[1] - start_color[1]) * y / 200
        b = start_color[2] + (end_color[2] - start_color[2]) * y / 200
        color = f'#{int(r):02x}{int(g):02x}{int(b):02x}'
        canvas.TKCanvas.create_line(0, y, 200, y, fill=color)


Msg_log = []

layout = [
                [
                    sg.Text("NerlNet Monitor" , key='-TEXT-' , size=(30,1) ,text_color='Black' , font=('SFPro' , 20) , background_color='#930707' , justification='center' , pad=(0,0))
                ] ,
                [   sg.Frame(title="Event Log:" , 
                            layout=[[sg.Multiline('', size=(140, 60), key='-LOG-', autoscroll=True , font=('SFPro' , 12) , no_scrollbar=True)]],
                            background_color=('#930707') , font=('SFPro' , 20) , size=(500,650) , title_color='Black' , element_justification='right') ,
                    sg.Text("Waiting For\n NerlNet Graph..." , key='-PHOLD-', text_color='Black' , font=('SFPro' , 12) , size=(70,5) , background_color='#930707' , justification='center' , pad=(0,0)) ,
                    sg.Image(key='-IMAGE-' , visible=False) 
                    
                ] ,
                [
                    sg.Button(button_text="Close" , button_color=('#C30404' , '#000000') , font=('SFPro' , 12) , size=(5,2)),
                    sg.Button(button_text="Clear Log" , button_color=('#C30404' , '#000000') , font=('SFPro' , 12) , size=(5,2))
                ]
                    
            ]

MainWindow = sg.Window("NErlNet" , layout , margins=(5,5) , size=(1400,800) , background_color='#930707' , finalize=True , resizable=True , element_justification='c')



def formatted_time():
    return f'[{datetime.now().day}/{datetime.now().month}/{datetime.now().year}|{datetime.now().hour}:{datetime.now().minute}:{datetime.now().second}]'

def GUI(MainPid):
    while True:
        event , values = MainWindow.read(timeout=100)
        updated_text = ''
        if event == "Close" or event == sg.WIN_CLOSED:
            os.kill(MainPid , 9)
            print("GUI Closed.")
            break
        if event == "Clear Log":
             MainWindow['-LOG-'].update('')
        if event == "NerlNet Graph":
             Show_Nerlnet_Graph()
        if not msg_queue.empty():
            msg = msg_queue.get_nowait()
            if msg[0] == 'graph':
                Show_Nerlnet_Graph(msg[1])
                MainWindow['-PHOLD-'].update(visible=False)
                MainWindow['-IMAGE-'].update(filename='NerlNetGraph.png' , visible=True , size=(800,600))
            elif values['-LOG-'] != '':
                existing_text = values['-LOG-']
                updated_text = f'{existing_text}\n{formatted_time()}: {msg}'
            else:
                updated_text = f'{formatted_time()}: {msg}'
            if updated_text != '':
                MainWindow['-LOG-'].update(updated_text)
        


    MainWindow.close()
    

def Show_Nerlnet_Graph(NerlGraph):
    # Graph in string format: "Entity1Name,Entity1IP,Entity1Port#Entity2Name,Entity2IP,Entity2Port#Entity1Name-Entity2Name,Entity2Name-Entity1Name#Worker1-Client1#Worker2-Client2" etc.
    # Workers in string format: "Worker1-Client1,Worker2-Client1,Worker3-Client2" etc.
    # Node is defined by a triplet 'Name,IP,Port' seperated by '#'
    # Edge is defined by a string 'Entity1-Entity2' seperated by ','
    Nodes = NerlGraph.split('#')[0:-1] 
    Edges = NerlGraph.split('#')[-1].split(',')[0:-1]
    Workers = NerlGraph.split('#')[-1].split(',')[-1].split('!')[0:-1]
    WorkersNames = [Worker.split('-')[0] for Worker in Workers]
    Edges += Workers
    EdgesSeperated = [(Edge.split('-')[0],Edge.split('-')[1]) for Edge in Edges if len(Edges) > 1] # ? What if no edges?
    NodesNames = [NodeTriplet.split(',')[0] for NodeTriplet in Nodes]
    NodesNames += WorkersNames

    graph = nx.Graph()
    graph.add_nodes_from(NodesNames)
    graph.add_edges_from(EdgesSeperated)
    pos = nx.spectral_layout(graph)
    angle = 40
    rotated_pos = {node: (x*math.cos(angle) -y*math.sin(angle), x*math.sin(angle) + y*math.cos(angle)) for node, (x, y) in pos.items()}
    plt.figure(figsize=(8,6))
    nx.draw_networkx(graph, rotated_pos, with_labels=True, node_color='skyblue', node_size=200, font_size=8, font_color='black' , edge_color='black' , width=1.5)
    plt.savefig('NerlNetGraph.png' ,bbox_inches='tight' , dpi=125)
    plt.close()


class MyProcess(Process):
        def __init__(self , msg_queue) -> None:
            Process.__init__(self)
            self.get_node().register_name(self, Atom('PyrlangProcess'))  
            self.msg_queue = msg_queue


        def handle_one_inbox_message(self, msg):
            print(f'From Pyrlang: {msg}')
            self.msg_queue.put(msg)
            if not self.msg_queue.empty():
                 print(f'Queue is not Empty: {msg} added.')
            

if __name__ == "__main__":
    msg_queue = multiprocessing.Queue()
    
    PyrlangPid = os.getpid()
    GUI_Process = multiprocessing.Process(target=GUI , args=(PyrlangPid,))
    GUI_Process.start()

    print("Starting a Pyrlang node...")
    PyNode = Node(node_name="py@127.0.0.1" , cookie="COOKIE")
    MyProcess(msg_queue)
    PyNode.run()

    


