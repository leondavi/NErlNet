#pragma once

#include <map>
#include <memory>
#include <string>
#include <vector>

#include <Logger.h>

namespace nerlnet
{
class NerlWorker
{
public:
    using WorkerParams = std::map<std::string, std::string>;

    NerlWorker(int distributed_system_type,
               std::string distributed_system_args_str,
               WorkerParams worker_params = {});
    virtual ~NerlWorker() = 0;

    int get_distributed_system_type() const { return _distributed_system_type; }
    const std::string &get_distributed_system_args() const { return _distributed_system_args_str; }
    const WorkerParams &get_worker_params() const { return _worker_params; }
    virtual std::shared_ptr<std::vector<int>> get_distributed_system_train_labels_count()
    {
        LogError << "Distributed System Weighted Avg count label is unsupported";
        throw("Distributed System Weighted Avg count label is unsupported");
    }

protected:
    WorkerParams _worker_params;
    int _distributed_system_type;
    std::string _distributed_system_args_str;
    std::shared_ptr<std::vector<int>> _train_labels_count;
};
}