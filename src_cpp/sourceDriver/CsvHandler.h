#pragma once
#include <fstream>
#include <Logger.h>

#define CSV_HANDLER_CONCCURENT_BUFFERS 3 //3 producers

namespace nerlnet{

class CsvHandler
{
private:
    std::vector<int> _start_row_num[CSV_HANDLER_CONCCURENT_BUFFERS]; //each thread knows where to start reading
    std::vector<bool> _data_ready[CSV_HANDLER_CONCCURENT_BUFFERS]; //each thread knows if the buffer is ready and can be tranmitted
    std::vector<bool> _terminate[CSV_HANDLER_CONCCURENT_BUFFERS]; //each thread knows if it should terminate
    std::vector<std::ifstream> _csv_file_streams[CSV_HANDLER_CONCCURENT_BUFFERS]; //each thread has its own file stream
    //define vector of strings where each string is a sample 
public:
    CsvHandler(std::string &csv_path, int batches_buffer_size, int batch_size);
    ~CsvHandler();
    void get_samples(int start_batch, int num_of_batches, std::vector<std::vector<std::string>> &samples); //samples are returned in the vector
    void prepare(); //prepare the buffer for reading (opens the threads)
    void terminate(); //terminate the threads
};







}