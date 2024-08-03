#include "torchNIF.h"


void* train_threaded_function(void* args)
{
    std::shared_ptr<dirty_thread_args>* p_thread_args_ptr = static_cast<std::shared_ptr<dirty_thread_args>*>(args);
    std::shared_ptr<dirty_thread_args> thread_args_ptr = *p_thread_args_ptr;
    delete p_thread_args_ptr;

    // TODO implement training
}


void* predict_threaded_function(void* args)
{
    std::shared_ptr<dirty_thread_args>* p_thread_args_ptr = static_cast<std::shared_ptr<dirty_thread_args>*>(args);
    std::shared_ptr<dirty_thread_args> thread_args_ptr = *p_thread_args_ptr;
    delete p_thread_args_ptr;

    // TODO implement prediction
}