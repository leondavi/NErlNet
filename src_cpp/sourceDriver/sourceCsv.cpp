#include SourceCsv.h
namespace nerlnet
{
    SourceCsv::SourceCsv(int type, int batch_id, int duration, std::vector<std::string> targets, int policy, int batch_size,
                        double desired_bps, std::string source_name, double supported_bps, int epochs,
                        int total_batches, std::string csv_path, bool no_dur_limit)
                        :sourceDriver(type, batch_id, duration, targets, policy, batch_size, desired_bps,
                         source_name, supported_bps)
    {
        _epochs = epochs;
        _total_batches = total_batches;
        _csv_path = csv_path;
        _no_dur_limit = no_dur_limit;
    }
    SourceCsv::~SourceCsv()
    {
    }
    void SourceCsv::load_csv(std::string &csv_path)
    {
        //load the csv file.
    }
    virtual int SourceCsv::get_batch() override
    {
        return _batch_size;
    }
}