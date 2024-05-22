#pragma once

#include "Source.h"

class SourceCSV : public Source
{
    SourceCSV();
    ~SourceCSV();

    public:

    void source_get_batches_nif() override;
    void source_more_batches_nif() override;
};