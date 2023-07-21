from term import Atom
from pyrlang.node import Node
from pyrlang.process import Process
import PySimpleGUI as sg
import multiprocessing
from time import sleep
import networkx as nx
import matplotlib.pyplot as plt
from datetime import datetime

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
                [   sg.Text("Waiting For\n NerlNet Graph..." , text_color='Black' , font=('SFPro' , 12) , size=(50,5) , background_color='#930707' , justification='center' , pad=(0,0)) , 
                    sg.Image(key='-IMAGE-' , visible=False , size=(500,400)) ,
                    sg.Frame(title="Event Log:" , 
                            layout=[[sg.Multiline('', size=(100, 20), key='-LOG-', autoscroll=True , font=('SFPro' , 12))]],
                            background_color=('#930707') , font=('SFPro' , 20) , size=(300,400) , title_color='Black' , element_justification='right')
                ] ,
                [
                    sg.Button(button_text="Close" , button_color=('#C30404' , '#000000') , font=('SFPro' , 12) , size=(5,2)),
                    sg.Button(button_text="Clear Log" , button_color=('#C30404' , '#000000') , font=('SFPro' , 12) , size=(5,2))
                ]
                    
            ]

MainWindow = sg.Window("NErlNet" , layout , margins=(5,5) , size=(800,500) , background_color='#930707' , finalize=True , resizable=False , element_justification='c')



def formatted_time():
    return f'[{datetime.now().day}/{datetime.now().month}/{datetime.now().year}|{datetime.now().hour}:{datetime.now().minute}:{datetime.now().second}]'

def GUI():
    while True:
        event , values = MainWindow.read(timeout=100)
        if event == "Close" or event == sg.WIN_CLOSED:
            break
        if event == "Clear Log":
             MainWindow['-LOG-'].update('')
        if event == "NerlNet Graph":
             Show_Nerlnet_Graph()
        if not msg_queue.empty():
            msg = msg_queue.get_nowait()
            if "Graph" in msg:
                Show_Nerlnet_Graph(msg)
            elif values['-LOG-'] != '':
                existing_text = values['-LOG-']
                updated_text = f'{existing_text}\n{formatted_time()}: {msg}'
            else:
                updated_text = f'{formatted_time()}: {msg}'
            MainWindow['-LOG-'].update(updated_text)
        


    MainWindow.close()

def Show_Nerlnet_Graph(NerlGraph):
    # Graph got in string format: "Entity1Name,Entity1IP,Entity1Port#Entity2Name,Entity2IP,Entity2Port#Entity1-Entity2,Entity2-Entity1" etc.
    # Node is defined by a triplet 'Name,IP,Port' seperated by '#'
    # Edge is defined by a string 'Entity1-Entity2' seperated by ','

    Nodes = NerlGraph.split('#')[0:-1]
    Edges = NerlGraph.split('#')[-1].split(',')
    EdgesSeperated = [(Edge.split('-')[0],Edge.split('-')[1]) for Edge in Edges if len(Edges) > 1]
    NodesNames = [NodeTriplet.split(',')[0] for NodeTriplet in Nodes]

    graph = nx.Graph()
    graph.add_nodes_from(NodesNames)
    graph.add_edges_from(EdgesSeperated)

    spring_pos = nx.spring_layout(graph)
    nx.draw_networkx(graph, spring_pos, with_labels=True, node_color='lightblue', node_size=500, font_size=12, font_color='black')
    plt.axis('off')
    plt.show()


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

    GUI_Process = multiprocessing.Process(target=GUI)
    GUI_Process.start()

    print("Starting a Pyrlang node...")
    PyNode = Node(node_name="py@127.0.0.1" , cookie="COOKIE")
    MyProcess(msg_queue)
    PyNode.run()

    

    
