import json

in1 = ''
in2 = ''
in3 = ''
in4 = ''


def creat():
  def get_connectionsMap_client_headr():
    print("ConnectionsMap CLIENTS")
    global in1
    in1 = input("Enter header client name:")
    return in1

  def get_connectionsMap_server_headr():
    print("ConnectionsMap SERVERS")
    global in2
    in2 = input("Enter server name:")
    return in2

  def get_connectionsMap_mainServer_headr():
    print("ConnectionsMap MAIN SERVER")
    global in3
    in3 = input("Enter mainServer Name:")
    return in3

  def get_connectionsMap_serverApi_headr():
    print("ConnectionsMap SERVER API")
    global in4
    in4 = input("Enter server api Name:")
    return in4

  def get_devices_inputs():
    d = {'host': input("Enter host:"), 'entities': input('Enter entities list:')}
    return d

  def get_serverAPI_inputs():
    c = {'host': input('Host:'), 'port': input('Port:'), 'args': input('Enter args list:')}
    return c

  def get_mainServer_inputs():
    g = {'host': input('Host:'), 'port': input('Port:'), 'args': input('Enter args list:')}
    return g

  def get_worker_inputs():
    f = {'name': input("Enter worker (cppSANNStatem) name:"), 'args': input('Enter args list:')}
    return f

  def get_client_inputs():
    t = {'name': input("Enter client name:"), 'port': input('Port:'), 'workers': input('Enter workers List:')}
    return t

  def get_source_inputs():
    l = {'name': input("Enter source name:"), 'port': input('Port:')}
    return l

  def get_router_inputs():
    s = {'name': input("Enter Machine Name:"), 'host': input('Host:'), 'port': input('Port:')}
    return s

  def get_connectionsMap_client():
    print("ConnectionsMap CLIENTS arguments")
    x1 = {'mainServer': input("Enter mainServer name:")}
    return x1

  def get_connectionsMap_server():
    print("ConnectionsMap SERVERS arguments")
    x2 = {in3: input("Enter mainServer name:"), in1: input("Enter client name:")}
    return x2

  def get_connectionsMap_mainServer():
    print("ConnectionsMap MAIN SERVER arguments")
    x3 = {in4: input("Enter mainServer name:"),
          in1: input("Enter server name:"),
          in2: input("Enter client name:")}
    return x3

  def get_connectionsMap_serverApi():
    print("ConnectionsMap SERVER API arguments")
    x4 = {in3: input("Enter mainServer name:")}
    return x4

  out = {"devices": [],
         "serverAPI": [],
         "mainServer": [],
         "cppSANNStatem": [],
         "clients": [],
         "sources": [],
         "routers": [],
         "connectionsMap": [{in1: {}}, {in2: {}}, {in3: {}}, {in4: {}}]
         }

  while True:
    exit = input('Do you want to add another device (y/n)? ')
    if exit.lower() == 'n':
      break
    else:
      d = get_devices_inputs()
      out["devices"].append(d)

  while True:
    exit = input('Do you want to add another serverAPI (y/n)? ')
    if exit.lower() == 'n':
      break
    else:
      c = get_serverAPI_inputs()
      out["serverAPI"].append(c)

  while True:
    exit = input('Do you want to add another mainServer (y/n)? ')
    if exit.lower() == 'n':
      break
    else:
      g = get_mainServer_inputs()
      out["mainServer"].append(g)

  while True:
    exit = input('Do you want to add another worker (y/n)? ')
    if exit.lower() == 'n':
      break
    else:
      f = get_worker_inputs()
      out["cppSANNStatem"].append(f)

  while True:
    exit = input('Do you want to add another client (y/n)? ')
    if exit.lower() == 'n':
      break
    else:
      t = get_client_inputs()
      out["clients"].append(t)

  while True:
    exit = input('Do you want to add another source (y/n)? ')
    if exit.lower() == 'n':
      break
    else:
      l = get_source_inputs()
      out["sources"].append(l)

  while True:
    exit = input('Do you want to add another router (y/n)? ')
    if exit.lower() == 'n':
      break
    else:
      s = get_router_inputs()
      out["routers"].append(s)

  exit = input('Please add you connectionMap')
  get_connectionsMap_client_headr()
  get_connectionsMap_server_headr()
  get_connectionsMap_mainServer_headr()
  get_connectionsMap_serverApi_headr()

  y1 = get_connectionsMap_client()
  out['connectionsMap'][0][in1] = out['connectionsMap'][0]['']
  del out['connectionsMap'][0]['']
  out["connectionsMap"][0][in1] = y1

  y2 = get_connectionsMap_server()
  out['connectionsMap'][1][in2] = out['connectionsMap'][1]['']
  del out['connectionsMap'][1]['']
  out["connectionsMap"][1][in2] = y2

  y3 = get_connectionsMap_mainServer()
  out['connectionsMap'][2][in3] = out['connectionsMap'][2]['']
  del out['connectionsMap'][2]['']
  out["connectionsMap"][2][in3] = y3

  y4 = get_connectionsMap_serverApi()
  out['connectionsMap'][3][in4] = out['connectionsMap'][3]['']
  del out['connectionsMap'][3]['']
  out["connectionsMap"][3][in4] = y4

  with open('architectures.json', 'w') as f:
    json.dump(out, f, indent=4)
    created = True
    return created
