from NerlDatasetDB import *

nerlnet_csv_db = DataSetCsvDB("/home/nerlnet/nerlnet/nerlnet/src_py/apiServer/nerlnet_csv_db.csv", "/home/nerlnet/nerlnet/nerlnet/src_py/apiServer/nerlnet_csv_db_temp", 50)

nerlnet_csv_db.add_source_db("source1","source1_db", "w1,w2,worker3", 0, 100, DataSetCsvDB.PHASE_TRAINING)
nerlnet_csv_db.add_source_db("source2","source_db2" "w1,w2,worker3", 0, 100, DataSetCsvDB.PHASE_TRAINING)
nerlnet_csv_db.generate_data_for_source("source1")