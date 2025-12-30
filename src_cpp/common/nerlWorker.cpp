#include "nerlWorker.h"

#include <utility>

namespace nerlnet
{

NerlWorker::NerlWorker(int distributed_system_type,
                       std::string distributed_system_args_str,
                       WorkerParams worker_params)
    : _worker_params(std::move(worker_params)),
      _distributed_system_type(distributed_system_type),
      _distributed_system_args_str(std::move(distributed_system_args_str))
{
}

NerlWorker::~NerlWorker() = default;

} // namespace nerlnet
