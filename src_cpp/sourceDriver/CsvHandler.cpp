#include "CsvHandler.h"


namespace nerlnet{

    CsvHandler::CsvHandler(std::string &csv_path, int batches_buffer_size, int batch_size)
    {
        _csv_path = csv_path;
        _batches_buffer_size = batches_buffer_size;
        _batch_size = batch_size;
    }



}