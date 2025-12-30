#include "TensorCodec.h"

#include <cstdint>
#include <stdexcept>
#include <string>

#include "nifppNerlTensorTorch.h"

namespace nerlnet::torchbridge
{
namespace
{
    template<typename BasicType>
    TorchTensor decode_with_type(ErlNifEnv* env, ERL_NIF_TERM nerltensor_term, c10::ScalarType dtype)
    {
        TorchTensor tensor;
        nifpp::get_nerltensor<BasicType>(env, nerltensor_term, tensor, dtype);
        return tensor;
    }

    template<typename BasicType>
    void encode_with_type(ErlNifEnv* env, TorchTensor& tensor, nifpp::TERM& encoded)
    {
        nifpp::make_tensor<BasicType>(env, encoded, tensor);
    }

    std::string unsupported_message(const char* action, c10::ScalarType dtype)
    {
        return std::string("Unsupported ") + action + " dtype: " + std::to_string(static_cast<int>(dtype));
    }
}

TorchTensor TensorCodec::decode(ErlNifEnv* env, ERL_NIF_TERM nerltensor_term, c10::ScalarType dtype)
{
    switch (dtype)
    {
        case c10::ScalarType::Float:
            return decode_with_type<float>(env, nerltensor_term, dtype);
        case c10::ScalarType::Double:
            return decode_with_type<double>(env, nerltensor_term, dtype);
        case c10::ScalarType::Int:
            return decode_with_type<int32_t>(env, nerltensor_term, dtype);
        case c10::ScalarType::Long:
            return decode_with_type<int64_t>(env, nerltensor_term, dtype);
        case c10::ScalarType::Short:
            return decode_with_type<int16_t>(env, nerltensor_term, dtype);
        case c10::ScalarType::Byte:
            return decode_with_type<uint8_t>(env, nerltensor_term, dtype);
        case c10::ScalarType::Char:
            return decode_with_type<int8_t>(env, nerltensor_term, dtype);
        case c10::ScalarType::Bool:
            return decode_with_type<bool>(env, nerltensor_term, dtype);
        default:
            throw std::invalid_argument(unsupported_message("decode", dtype));
    }
}

void TensorCodec::encode(ErlNifEnv* env, const TorchTensor& tensor, nifpp::TERM& encoded)
{
    TorchTensor prepared = ensure_host_contiguous(tensor);
    switch (prepared.scalar_type())
    {
        case c10::ScalarType::Float:
            encode_with_type<float>(env, prepared, encoded);
            break;
        case c10::ScalarType::Double:
            encode_with_type<double>(env, prepared, encoded);
            break;
        case c10::ScalarType::Int:
            encode_with_type<int32_t>(env, prepared, encoded);
            break;
        case c10::ScalarType::Long:
            encode_with_type<int64_t>(env, prepared, encoded);
            break;
        case c10::ScalarType::Short:
            encode_with_type<int16_t>(env, prepared, encoded);
            break;
        case c10::ScalarType::Byte:
            encode_with_type<uint8_t>(env, prepared, encoded);
            break;
        case c10::ScalarType::Char:
            encode_with_type<int8_t>(env, prepared, encoded);
            break;
        case c10::ScalarType::Bool:
            encode_with_type<bool>(env, prepared, encoded);
            break;
        default:
            throw std::invalid_argument(unsupported_message("encode", prepared.scalar_type()));
    }
}

TorchTensor TensorCodec::ensure_host_contiguous(const TorchTensor& tensor)
{
    TorchTensor host_tensor = tensor;
    if (!tensor.device().is_cpu())
    {
        host_tensor = tensor.to(torch::kCPU);
    }
    if (!host_tensor.is_contiguous())
    {
        host_tensor = host_tensor.contiguous();
    }
    return host_tensor;
}

} // namespace nerlnet::torchbridge
