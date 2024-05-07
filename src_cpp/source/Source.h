#pragma once

class Source
{   
    public:

    virtual ~Source() = default;

    virtual void source_get_batches_nif() = 0;
    virtual void source_more_batches_nif() = 0;
};