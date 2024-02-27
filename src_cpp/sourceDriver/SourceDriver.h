#pragma once
#include <memory>
#include <vector>
#include "common_definitions.h"
#include "utilities.h"

namespace nerlnet
{
class SourceDriver
{
    public:
    SourceDriver(int type, int batch_id, int duration, std::vector<std::string> targets, int policy, int batch_size,
                 double desired_bps, std::string source_name, double supported_bps);
    ~SourceDriver();             
    virtual bool is_ready() = 0;
    virtual std::shared_ptr<char> get_batch() = 0;

    protected:
    int _type;
    int _batch_id;
    int _duration;
    std::vector<std::string> _targets;
    int _policy;
    int _batch_size;
    double _desired_bps;
    std::string _source_name;
    double _supported_bps;

    private:
};

} // namespace nerlnet