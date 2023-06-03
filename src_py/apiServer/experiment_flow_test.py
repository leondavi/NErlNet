
from apiServer import *

print("Experiment Flow Test")

api_server_instance = ApiServer()
api_server_instance.help()

api_server_instance.showJsons()
api_server_instance.setJsons(3,9,2)

arch_json , connmap_json, exp_flow_json = api_server_instance.getUserJsons()

api_server_instance.initialization(arch_json , connmap_json, exp_flow_json)
api_server_instance.sendJsonsToDevices()

api_server_instance.sendDataToSources("Training")
raise "hey"
api_server_instance.train()

api_server_instance.sendDataToSources("Prediction")
api_server_instance.predict()

api_server_instance.plot_loss(1)
api_server_instance.accuracy_matrix(1)
#api_server_instance.statistics()
