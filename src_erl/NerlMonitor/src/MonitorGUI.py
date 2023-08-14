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
import asyncio
import nest_asyncio
nest_asyncio.apply()


class MyProcess(Process):
        def __init__(self , msg_queue) -> None:
            Process.__init__(self)
            self.get_node().register_name(self, Atom('PyrlangProcess'))  
            self.msg_queue = msg_queue


        def handle_one_inbox_message(self, msg):
            print(f'From ErlProcess: {msg}')
            if msg[0] == Atom('send'):
                print(f'From ErlProcess: {msg[1]}')
            else:
                self.msg_queue.put_nowait(msg)
            if not self.msg_queue.empty():
                 print(f'Queue is not Empty: {msg} added.')
     



def draw_gradient(canvas, start_color, end_color):
    for y in range(0, 200):  # Adjust the range to your desired height
        r = start_color[0] + (end_color[0] - start_color[0]) * y / 200
        g = start_color[1] + (end_color[1] - start_color[1]) * y / 200
        b = start_color[2] + (end_color[2] - start_color[2]) * y / 200
        color = f'#{int(r):02x}{int(g):02x}{int(b):02x}'
        canvas.TKCanvas.create_line(0, y, 200, y, fill=color)


Msg_log = []

DataColumn = [
                [sg.Frame(title="Event Log:" , 
                            layout=[[sg.Multiline('', size=(140, 60), key='-LOG-', autoscroll=True , font=('SFPro' , 12) , no_scrollbar=True)]],
                            background_color=('#A90433') , font=('SFPro' , 20) , size=(500,325) , title_color='White' , element_justification='right')
                ] ,
                [sg.Frame(title="Statistics:" , 
                            layout=[[sg.Multiline('', size=(140, 60), key='-STATS-', autoscroll=True , font=('SFPro' , 12) , no_scrollbar=True)]],
                            background_color=('#A90433') , font=('SFPro' , 20) , size=(500,325) , title_color='White' , element_justification='right')
                ]
             ]

GraphColumn = [
                [   sg.Text("Waiting For\n NerlNet Graph..." , key='-PHOLD-', text_color='White' , font=('SFPro' , 12) , size=(70,5) , background_color='#A90433' , justification='center' , pad=(0,0)) ,
                    sg.Image(key='-IMAGE-' , visible=False) 
                ],
                [   
                    sg.Text("Enter the name of the worker you wish to terminate:" ,key='-INTEXT-', size=(42,1) ,text_color='white' , font=('SFPro' , 12) , background_color='#A90433' , justification='left' , pad=(0,0) , visible=False) , 
                    sg.Input('',key='-INPUT-' , visible=False , justification='left' , size=(20,1) , font=('SFPro' , 12) , background_color='white' , text_color='black' , pad=(0,0) , enable_events=True),
                    sg.Button(button_text="Terminate" , button_color=('#A90433' , '#FFFFFF') , font=('SFPro' , 12) , size=(10,1) , pad=(0,0) , visible=False, key='-TERM-', enable_events=True)
                ]
              ]

layout = [
                [
                    sg.Text("NerlNet Monitor" , key='-TEXT-' , size=(30,1) ,text_color='White' , font=('SFPro' , 20) , background_color='#A90433' , justification='center' , pad=(0,0))
                ] ,
                [   sg.Column(DataColumn , background_color='#A90433') ,
                    sg.VSeperator() ,
                    sg.Column(GraphColumn , background_color='#A90433')
                ] ,
                [
                    sg.Button(button_text="Close" , button_color=('#A90433' , '#FFFFFF') , font=('SFPro' , 12) , size=(5,2)),
                    sg.Button(button_text="Clear Log" , button_color=('#A90433' , '#FFFFFF') , font=('SFPro' , 12) , size=(5,2))
                ]
                    
          ]

MainWindow = sg.Window("NErlNet" , layout , margins=(5,5) , size=(1400,800) , background_color='#A90433' , finalize=True , resizable=True , element_justification='c' , icon='../../../NerlnetLogo.ico')

def RemoteRecv():
    return Atom('erl@127.0.0.1') , Atom("recvPyrlang")

def formatted_time():
    return f'[{datetime.now().day}/{datetime.now().month}/{datetime.now().year}|{datetime.now().hour}:{datetime.now().minute}:{datetime.now().second}]'

# def SendMsg(Msg):
#     SendNode  = Node(node_name='pysend@127.0.0.1' , cookie='COOKIE')
#     SendProc = MyProcess()
#     event_loop = SendNode.get_loop()

#     print(SendNode.where_is(Atom('recvPyrlang')))

#     def task():
#         SendNode.send_nowait(sender = SendProc.pid_ , receiver = RemoteRecv() , message = (Atom('send'),Atom(Msg)))
#         SendNode.destroy()
    
#     event_loop.call_soon(task)

#     SendNode.run()

async def GUI(msg_queue):
    print("GUI task started...")
    PyNode , CommProc = await msg_queue.get()
    print("Got Message from queue")
    print(msg_queue.empty())
    print(f"Got PyNode and CommProc from Queue.")
    StatsInfo = {"workers": {} , "clients": {}}
    while True:
        await asyncio.sleep(.01)
        event , values = MainWindow.read(timeout=100)
        existing_text = values['-LOG-']
        updated_text = ''
        if event == "Close" or event == sg.WIN_CLOSED:
            PyNode.send_nowait(sender = CommProc.pid_ , receiver = RemoteRecv() , message = (Atom('close')))
            await asyncio.sleep(.2)
            os.kill(os.getpid() , 9)
            print("GUI Closed.")
            break
        elif event == "Clear Log":
            ShowStats(StatsInfo)
            MainWindow['-LOG-'].update('')
        elif event == "-TERM-":
            Workers = [Graph.nodes[node]['label'] for node in Graph.nodes() if Graph.nodes[node]['label'][0] == 'w' and node_colors[node] != 'gray']
            if values['-INPUT-'] not in Workers:
                updated_text = f'{existing_text}\n{formatted_time()}: Invalid Worker Name {values["-INPUT-"]} , Available Workers: {Workers}.'
                MainWindow['-LOG-'].update(updated_text)
            else:
                Workers.remove(values['-INPUT-'])
                node_colors[values['-INPUT-']] = 'gray'
                nx.set_node_attributes(Graph, node_colors, 'color')
                colors = nx.get_node_attributes(Graph, 'color').values()
                pos = nx.nx_agraph.graphviz_layout(Graph, prog='dot')
                plt.figure(figsize=(8,6))
                nx.draw_networkx(Graph, pos, with_labels=True, node_color=colors , node_size=200, font_size=8, font_color='white' , edge_color='black' , width=1.5)
                plt.savefig('NerlNetGraph.png' ,bbox_inches='tight' , dpi=125)
                plt.close()
                MainWindow['-IMAGE-'].update(filename='NerlNetGraph.png' , visible=True , size=(800,600))
                
                updated_text = f'{existing_text}\n{formatted_time()}: Sending termination message for {values["-INPUT-"]} to Main Server.'
                PyNode.send_nowait(sender = CommProc.pid_ , receiver = RemoteRecv() , message = (Atom('terminate'),Atom(f'{values["-INPUT-"]}')))


                MainWindow['-LOG-'].update(updated_text)
            MainWindow['-INPUT-'].update('')
        if not msg_queue.empty():
            msg = msg_queue.get_nowait()
            if msg[0] == 'graph':
                Graph , node_colors = Show_Nerlnet_Graph(msg[1])
                MainWindow['-PHOLD-'].update(visible=False)
                MainWindow['-IMAGE-'].update(filename='NerlNetGraph.png' , visible=True , size=(800,600))
                MainWindow['-LOG-'].update(f'{formatted_time()}: NerlNet Graph Received.')
                MainWindow['-INTEXT-'].update(visible=True)
                MainWindow['-INPUT-'].update(visible=True)
                MainWindow['-TERM-'].update(visible=True)
            elif msg[0] == 'update':
                ClientName , WorkerName = msg[1].split('-')

                node_colors[WorkerName] = 'gray'
                #node_colors[ClientName] = 'gray'
                nx.set_node_attributes(Graph, node_colors, 'color')
                colors = nx.get_node_attributes(Graph, 'color').values()

                pos = nx.nx_agraph.graphviz_layout(Graph, prog='dot')
                angle = 100
                #rotated_pos = {node: (x*math.cos(angle) -y*math.sin(angle), x*math.sin(angle) + y*math.cos(angle)) for node, (x, y) in pos.items()}

                plt.figure(figsize=(8,6))
                nx.draw_networkx(Graph, pos, with_labels=True, node_color=colors , node_size=200, font_size=8, font_color='white' , edge_color='black' , width=1.5)
                plt.savefig('NerlNetGraph.png' ,bbox_inches='tight' , dpi=125)
                plt.close()
                MainWindow['-IMAGE-'].update(filename='NerlNetGraph.png' , visible=True , size=(800,600))
                if existing_text == '':
                    updated_text = f'{formatted_time()}: Worker {WorkerName} of Client {ClientName} is down.'
                else:
                    Workers = [Graph.nodes[node]['label'] for node in Graph.nodes() if Graph.nodes[node]['label'][0] == 'w' and node_colors[node] != 'gray']
                    updated_text = f'{existing_text}\n{formatted_time()}: Worker {WorkerName} of Client {ClientName} is down , Available workers: {Workers}'
                MainWindow['-LOG-'].update(updated_text)
            
            elif msg[0] == 'stats':
                try:
                    Data = msg[1]
                    for items in str(Data).split('|'): 
                        Entity, val = items.split(':') 
                        if '=' in val:      
                            for EntityStat in val.split(','):
                                Stat, Result = EntityStat.split('=')
                                if "Train" in Stat:
                                    StatsInfo["workers"][Stat] = Result
                                else:
                                    StatsInfo["clients"][Stat] = Result
                        else:               
                            StatsInfo[Entity] = val # Messages for entities other than clients/workers
                    CurrentStats = StatsInfo.copy() # copy the stats to a new variable
                    StatsText = ShowStats(CurrentStats)
                    existing_stats = values['-STATS-']
                    if existing_stats != '':
                        MainWindow['-STATS-'].update(f'{existing_stats}\n{StatsText}')
                    else:
                        MainWindow['-STATS-'].update(StatsText)
                    existing_text = values['-LOG-']
                    MainWindow['-LOG-'].update(f'{existing_text}\n{formatted_time()}: Statistics Received.')
                except Exception as err:
                    MainWindow['-LOG-'].update(f"Error in Stats {err} , Got {StatsInfo}")
                

            elif values['-LOG-'] != '':
                existing_text = values['-LOG-']
                updated_text = f'{existing_text}\n{formatted_time()}: {msg}'
            else:
                updated_text = f'{formatted_time()}: {msg}'
            if updated_text != '':
                MainWindow['-LOG-'].update(updated_text)
        


    MainWindow.close()
    

def ShowStats(CurrentStats):
    MainWindow['-LOG-'].update(f'{formatted_time()}: Printing Statistics...')
    StatsText = ''
    for key in CurrentStats:
        if key == 'workers':
            StatsText += f'Workers:\n'
            for stat in CurrentStats[key]:
                if "Time" in stat:
                    StatsText += f'\t{stat.replace("_Train_" , " Working ")}: {CurrentStats[key][stat]} seconds\n'
                else:
                    StatsText += f'\t{stat.replace("_" , " ")}: {CurrentStats[key][stat]}\n'
        elif key == 'clients':
            StatsText += f'Clients:\n'
            for stat in CurrentStats[key]:
                if "info" in stat:
                    StatsText += f'\t{stat.replace("_info_" , " Info ")}: {CurrentStats[key][stat]} bytes\n'
                else:
                    StatsText += f'\t{stat.replace("_Msg_" , " Message ")}: {CurrentStats[key][stat]}\n'
        else:
            StatsText += f'{key} Message Count: {CurrentStats[key]}\n'
    return StatsText


def Show_Nerlnet_Graph(NerlGraph):
    # Graph in string format: "Entity1Name,Entity1IP,Entity1Port#Entity2Name,Entity2IP,Entity2Port#Entity1Name-Entity2Name,Entity2Name-Entity1Name#Worker1-Client1#Worker2-Client2" etc.
    # Workers in string format: "Worker1-Client1,Worker2-Client1,Worker3-Client2" etc.
    # Node is defined by a triplet 'Name,IP,Port' seperated by '#'
    # Edge is defined by a string 'Entity1-Entity2' seperated by ','
    Nodes = NerlGraph.split('#')[0:-1] 
    Edges = NerlGraph.split('#')[-1].split(',')[0:-1]
    Workers = NerlGraph.split('#')[-1].split(',')[-1].split('!')[0:-1]
    WorkersNames = [Worker.split('-')[0] for Worker in Workers ]
    Edges += Workers
    EdgesSeperated = [(Edge.split('-')[0],Edge.split('-')[1]) for Edge in Edges if len(Edges) > 1] # ? What if no edges?
    NodesNames = [NodeTriplet.split(',')[0] for NodeTriplet in Nodes]
    NodesNames += WorkersNames

    NodeWithLabels = [(NodeName , {'label' : NodeName}) for NodeName in NodesNames]
    graph = nx.Graph()
    graph.add_nodes_from(NodeWithLabels)
    graph.add_edges_from(EdgesSeperated)

    my_labels = {'mainServer': 'mS' , 'apiServer': 'aS'}

    nx.relabel_nodes(graph, my_labels , copy=False)
    default_colors = {node:'#A90433' for node in graph.nodes()}
    node_colors = {node:default_colors[node] for node in graph.nodes()}
    nx.set_node_attributes(graph, node_colors, 'color')
    colors = nx.get_node_attributes(graph, 'color').values()

    pos = nx.nx_agraph.graphviz_layout(graph, prog='dot')
    angle = 100
    #rotated_pos = {node: (x*math.cos(angle) -y*math.sin(angle), x*math.sin(angle) + y*math.cos(angle)) for node, (x, y) in pos.items()}
    plt.figure(figsize=(8,6))
    nx.draw_networkx(graph, pos, with_labels=True, node_color=colors , node_size=200, font_size=8, font_color='white' , edge_color='black' , width=1.5)
    plt.savefig('NerlNetGraph.png' ,bbox_inches='tight' , dpi=125)
    plt.close()
    return graph , node_colors



async def Pyrlang(msg_queue):
    print("Pyrlang task started...")
    PyNode = Node(node_name="py@127.0.0.1" , cookie="COOKIE")
    CommProc = MyProcess(msg_queue=msg_queue)
    msg_queue.put_nowait((PyNode , CommProc))
    print("Pyrlang task finished.")
    PyNode.run()
 
async def main_func():
    msg_queue = asyncio.Queue()
    await asyncio.gather(GUI(msg_queue) , Pyrlang(msg_queue))

if __name__ == "__main__":
    asyncio.run(main_func())

    


