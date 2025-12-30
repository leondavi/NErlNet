#pragma once

#include <nifpp.h>

#include "nerltensorTorchDefs.h"

namespace nerlnet::torchbridge
{
class TensorCodec
{
public:
    static TorchTensor decode(ErlNifEnv* env, ERL_NIF_TERM nerltensor_term, c10::ScalarType dtype);
    static void encode(ErlNifEnv* env, const TorchTensor& tensor, nifpp::TERM& encoded);

private:
    static TorchTensor ensure_host_contiguous(const TorchTensor& tensor);
};
}
