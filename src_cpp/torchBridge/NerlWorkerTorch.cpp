#include "NerlWorkerTorch.h"

#include <stdexcept>
#include <vector>
#include <algorithm>
#include <cctype>
#include <utility>
#include <sstream>

#include <torch/optim.h>

namespace
{
std::string &blank_string()
{
	static std::string empty;
	return empty;
}
}

namespace nerlnet
{

namespace
{
	TorchTensor clone_tensor(const TorchTensor &src)
	{
		if (!src.defined())
		{
			throw std::invalid_argument("Undefined tensor passed to torch worker");
		}
		return src.clone();
	}
}

	int64_t NerlWorkerTorch::BatchLayout::required_elements() const
	{
		return sample_elem_span;
	}

	bool NerlWorkerTorch::BatchLayout::valid() const
	{
		return input_elem_count > 0 && label_elem_count > 0 && sample_elem_span > 0 && input_sample_span > 0 && label_sample_span > 0 && !input_shape.empty() && !label_shape.empty();
	}

	NerlWorkerTorch::NerlWorkerTorch(int distributed_system_type,
						 std::string distributed_system_args_str,
						 TrainingParams training_params)
		: NerlWorker(distributed_system_type,
					 std::move(distributed_system_args_str),
					 training_params),
		_training_params(std::move(training_params))
	{
		LogInfo << "Torch worker ctor start ds_type=" << distributed_system_type
				<< " param_count=" << _training_params.size() << std::endl;
		for (const auto &Entry : _training_params)
		{
			LogInfo << "Torch worker param key='" << Entry.first << "'" << std::endl;
		}
		load_script_module();
		LogInfo << "Torch worker script module load finished (has_module=" << _has_script_module << ")" << std::endl;
		initialize_training_config();
		LogInfo << "Torch worker training config initialized" << std::endl;
	}

	void NerlWorkerTorch::load_script_module()
	{
		std::string resolved_path = get_param_or_default({"model_path"}, "");
		if (resolved_path.empty())
		{
			LogWarning << "Torch worker created without a model_path train_param" << std::endl;
			return;
		}

		LogInfo << "Torch worker attempting to load TorchScript model from '" << resolved_path << "'" << std::endl;
		try
		{
			_script_module = torch::jit::load(resolved_path);
			_script_module.eval();
			_has_script_module = true;
			_model_path = resolved_path;
			LogInfo << "Loaded TorchScript model from " << resolved_path << std::endl;
		}
		catch (const std::exception &ex)
		{
			LogWarning << "Failed to load TorchScript model from " << resolved_path << ": " << ex.what() << std::endl;
		}
	}

	void NerlWorkerTorch::initialize_training_config()
	{
		_configured_learning_rate = get_float_param({"lr", "learning_rate"}, 0.0F);
		_configured_epochs = get_int_param({"epochs"}, 1);
		_optimizer_name = to_lower_copy(get_param_or_default({"optimizer", "optim"}, "sgd"));
		_randomize_weights_on_load = get_bool_param({"w_init_rand"}, false);
		if (_configured_epochs < 1)
		{
			_configured_epochs = 1;
		}

		if (_randomize_weights_on_load)
		{
			maybe_randomize_module_weights();
		}

		initialize_batch_layout();
		initialize_optimizer();

		LogInfo << "Torch worker configured with lr=" << _configured_learning_rate
				<< ", epochs=" << _configured_epochs
				<< ", optimizer=" << _optimizer_name
			<< ", randomize_on_load=" << _randomize_weights_on_load
				<< ", has_layout=" << _has_batch_layout
				<< ", has_optimizer=" << _has_optimizer << std::endl;
	}

	void NerlWorkerTorch::initialize_batch_layout()
	{
		auto input_shape = parse_shape_param({"input_tensor_shape"});
		auto label_shape = parse_shape_param({"labels_shape"});
		if (input_shape.empty() || label_shape.empty())
		{
			LogWarning << "Torch worker missing input/label shape metadata in train_params" << std::endl;
			_has_batch_layout = false;
			return;
		}

		BatchLayout layout;
		layout.input_shape = std::move(input_shape);
		layout.label_shape = std::move(label_shape);
		layout.input_elem_count = count_elements(layout.input_shape);
		layout.label_elem_count = count_elements(layout.label_shape);
		if (layout.input_elem_count <= 0 || layout.label_elem_count <= 0)
		{
			LogWarning << "Torch worker received non-positive element counts for input/label shapes" << std::endl;
			_has_batch_layout = false;
			return;
		}

		layout.expected_batch_size = layout.input_shape.front();
		const int64_t label_expected_batch = layout.label_shape.front();
		if (layout.expected_batch_size <= 0 || label_expected_batch <= 0)
		{
			LogWarning << "Torch worker received non-positive batch dimension in shape metadata" << std::endl;
			_has_batch_layout = false;
			return;
		}
		if (layout.expected_batch_size != label_expected_batch)
		{
			LogWarning << "Torch worker input/label shapes disagree on batch dimension" << std::endl;
			_has_batch_layout = false;
			return;
		}

		layout.input_sample_span = count_suffix_elements(layout.input_shape);
		layout.label_sample_span = count_suffix_elements(layout.label_shape);
		if (layout.input_sample_span <= 0 || layout.label_sample_span <= 0)
		{
			LogWarning << "Torch worker unable to derive per-sample span from shape metadata" << std::endl;
			_has_batch_layout = false;
			return;
		}
		if (layout.input_sample_span * layout.expected_batch_size != layout.input_elem_count ||
			layout.label_sample_span * layout.expected_batch_size != layout.label_elem_count)
		{
			LogWarning << "Torch worker shape metadata inconsistent with element counts" << std::endl;
			_has_batch_layout = false;
			return;
		}

		const std::string offset_spec = get_param_or_default({"labels_offset"}, "default");
		layout.labels_offset = resolve_labels_offset(offset_spec, layout.input_sample_span);
		if (layout.labels_offset < 0)
		{
			layout.labels_offset = layout.input_sample_span;
		}

		const int64_t labels_end = layout.labels_offset + layout.label_sample_span;
		layout.sample_elem_span = std::max(layout.input_sample_span, labels_end);

		_batch_layout = layout;
		_has_batch_layout = _batch_layout.valid();
		if (!_has_batch_layout)
		{
			LogWarning << "Torch worker computed invalid batch layout" << std::endl;
		}
	}

	void NerlWorkerTorch::initialize_optimizer()
	{
		if (!_has_script_module)
		{
			LogWarning << "Torch worker cannot build optimizer without a script module" << std::endl;
			_has_optimizer = false;
			return;
		}

		if (_configured_learning_rate <= 0.0F)
		{
			_configured_learning_rate = 1.0e-3F;
		}

		try
		{
			_trainable_parameters.clear();
			for (const auto &named_param : _script_module.named_parameters(/*recurse=*/true))
			{
				_trainable_parameters.push_back(named_param.value);
			}
			if (_trainable_parameters.empty())
			{
				LogWarning << "Torch worker found no trainable parameters in script module" << std::endl;
				_has_optimizer = false;
				return;
			}

			if (_optimizer_name == "adam")
			{
				torch::optim::AdamOptions options(_configured_learning_rate);
				_optimizer = std::make_unique<torch::optim::Adam>(_trainable_parameters, options);
			}
			else
			{
				torch::optim::SGDOptions options(_configured_learning_rate);
				options.momentum(0.9);
				_optimizer = std::make_unique<torch::optim::SGD>(_trainable_parameters, options);
			}
			_has_optimizer = static_cast<bool>(_optimizer);
		}
		catch (const std::exception &ex)
		{
			LogWarning << "Torch worker failed to build optimizer: " << ex.what() << std::endl;
			_has_optimizer = false;
		}
	}

TorchTensor NerlWorkerTorch::ensure_training_dtype(const TorchTensor &batch) const
{
	auto options = batch.options();
	if (options.dtype() == torch::kFloat || options.dtype() == torch::kDouble)
	{
		return batch;
	}
	return batch.to(torch::kFloat);
}

NerlWorkerTorch::TrainingSlices NerlWorkerTorch::split_training_batch(const TorchTensor &prepared) const
{
	if (!_has_batch_layout)
	{
		throw std::runtime_error("Torch worker batch layout not initialized");
	}

	TorchTensor contiguous = prepared.contiguous();
	TorchTensor flattened = contiguous.reshape({contiguous.numel()});
	const int64_t sample_span = _batch_layout.required_elements();
	if (sample_span <= 0)
	{
		throw std::runtime_error("Torch worker invalid sample span");
	}
	if (flattened.numel() % sample_span != 0)
	{
		std::ostringstream oss;
		oss << "Torch worker batch elements (" << flattened.numel()
			<< ") not divisible by sample span (" << sample_span << ")";
		throw std::runtime_error(oss.str());
	}

	const int64_t sample_count = flattened.numel() / sample_span;
	if (sample_count <= 0)
	{
		throw std::runtime_error("Torch worker derived non-positive sample count");
	}

	TorchTensor sample_matrix = flattened.reshape({sample_count, sample_span});
	if (_batch_layout.input_sample_span > sample_span)
	{
		std::ostringstream oss;
		oss << "Torch worker input span (" << _batch_layout.input_sample_span
			<< ") exceeds sample span (" << sample_span << ")";
		throw std::runtime_error(oss.str());
	}
	if (_batch_layout.labels_offset + _batch_layout.label_sample_span > sample_span)
	{
		std::ostringstream oss;
		oss << "Torch worker labels exceed sample span. offset=" << _batch_layout.labels_offset
			<< " label_span=" << _batch_layout.label_sample_span
			<< " sample_span=" << sample_span;
		throw std::runtime_error(oss.str());
	}

	TorchTensor input_matrix = sample_matrix.narrow(1, 0, _batch_layout.input_sample_span);
	TorchTensor label_matrix = sample_matrix.narrow(1, _batch_layout.labels_offset, _batch_layout.label_sample_span);

	std::vector<int64_t> input_view_shape = _batch_layout.input_shape;
	if (!input_view_shape.empty())
	{
		input_view_shape[0] = sample_count;
	}
	else
	{
		input_view_shape.push_back(sample_count);
	}
	std::vector<int64_t> label_view_shape = _batch_layout.label_shape;
	if (!label_view_shape.empty())
	{
		label_view_shape[0] = sample_count;
	}
	else
	{
		label_view_shape.push_back(sample_count);
	}

	TorchTensor inputs = input_matrix.reshape(torch::IntArrayRef(input_view_shape));
	TorchTensor labels = label_matrix.reshape(torch::IntArrayRef(label_view_shape));
	return {inputs, labels};
}

TorchTensor NerlWorkerTorch::train_batch(const TorchTensor &batch)
{
	TorchTensor prepared = ensure_training_dtype(batch);
	if (!_has_batch_layout)
	{
		LogWarning << "Torch worker missing batch layout metadata; returning placeholder loss" << std::endl;
		TorchTensor loss = prepared.mean().unsqueeze(0);
		_last_loss = loss.clone();
		_last_prediction = prepared.clone();
		return loss;
	}

	TrainingSlices slices = split_training_batch(prepared);
	if (!_has_script_module || !_has_optimizer)
	{
		LogWarning << "Torch worker missing script module or optimizer; returning placeholder loss" << std::endl;
		TorchTensor loss = slices.inputs.mean().unsqueeze(0);
		_last_loss = loss.clone();
		_last_prediction = slices.inputs.clone();
		return loss;
	}

	try
	{
		_optimizer->zero_grad();
		TorchTensor prediction = forward_or_clone(slices.inputs, true);
		if (!prediction.defined())
		{
			throw std::runtime_error("Torch worker forward pass returned undefined tensor");
		}
		if (prediction.scalar_type() != slices.labels.scalar_type())
		{
			prediction = prediction.to(slices.labels.scalar_type());
		}
		if (prediction.numel() != slices.labels.numel())
		{
			std::ostringstream oss;
			oss << "Torch worker prediction element mismatch. pred=" << prediction.numel()
				<< " labels=" << slices.labels.numel();
			throw std::runtime_error(oss.str());
		}
		if (prediction.sizes() != slices.labels.sizes())
		{
			prediction = prediction.reshape(slices.labels.sizes());
		}

		TorchTensor loss = torch::mse_loss(prediction, slices.labels);
		loss.backward();
		_optimizer->step();

		_last_loss = loss.detach();
		_last_prediction = prediction.detach();
		return _last_loss.clone();
	}
	catch (const std::exception &ex)
	{
		LogWarning << "Torch worker training failed: " << ex.what() << std::endl;
		TorchTensor fallback = slices.inputs.mean().unsqueeze(0);
		_last_loss = fallback.clone();
		_last_prediction = slices.inputs.clone();
		return fallback;
	}
}

TorchTensor NerlWorkerTorch::predict_batch(const TorchTensor &batch)
{
	TorchTensor prepared = ensure_training_dtype(batch);
	TorchTensor prediction = forward_or_clone(prepared, false);
	_last_prediction = prediction.clone();
	return prediction;
}

TorchTensor NerlWorkerTorch::forward_or_clone(const TorchTensor &input, bool training_mode)
{
	if (_has_script_module)
	{
		try
		{
			if (training_mode)
			{
				_script_module.train();
			}
			else
			{
				_script_module.eval();
			}
			std::vector<torch::jit::IValue> inputs;
			inputs.emplace_back(input);
			torch::jit::IValue output = _script_module.forward(inputs);
			if (output.isTensor())
			{
				return output.toTensor();
			}
			LogWarning << "TorchScript model returned non-tensor output" << std::endl;
		}
		catch (const std::exception &ex)
		{
			LogWarning << "TorchScript forward failed: " << ex.what() << std::endl;
		}
	}
	return clone_tensor(input);
}

	std::vector<int64_t> NerlWorkerTorch::parse_shape_param(const std::initializer_list<const char *> &keys) const
	{
		const std::string spec = get_param_or_default(keys, "");
		if (spec.empty())
		{
			return {};
		}
		return parse_shape_spec(spec);
	}

	std::vector<int64_t> NerlWorkerTorch::parse_shape_spec(const std::string &text)
	{
		std::vector<int64_t> dims;
		std::string token;
		auto flush_token = [&]() {
			if (!token.empty())
			{
				dims.push_back(std::stoll(token));
				token.clear();
			}
		};

		for (char ch : text)
		{
			if ((ch >= '0' && ch <= '9') || ch == '-' || ch == '+')
			{
				token.push_back(ch);
			}
			else
			{
				flush_token();
			}
		}
		flush_token();

		return dims;
	}

	int64_t NerlWorkerTorch::count_elements(const std::vector<int64_t> &dims)
	{
		if (dims.empty())
		{
			return 0;
		}
		int64_t total = 1;
		for (int64_t dim : dims)
		{
			if (dim <= 0)
			{
				return 0;
			}
			total *= dim;
		}
		return total;
	}

	int64_t NerlWorkerTorch::count_suffix_elements(const std::vector<int64_t> &dims)
	{
		if (dims.empty())
		{
			return 0;
		}
		if (dims.size() == 1)
		{
			return 1;
		}
		int64_t total = 1;
		for (size_t i = 1; i < dims.size(); ++i)
		{
			const int64_t dim = dims[i];
			if (dim <= 0)
			{
				return 0;
			}
			total *= dim;
		}
		return total;
	}

	int64_t NerlWorkerTorch::resolve_labels_offset(const std::string &value, int64_t default_offset) const
	{
		if (value.empty() || value == "default")
		{
			return default_offset;
		}
		try
		{
			return std::stoll(value);
		}
		catch (const std::exception &)
		{
			LogWarning << "Torch worker failed to parse labels_offset='" << value << "', using default" << std::endl;
			return default_offset;
		}
	}

std::string NerlWorkerTorch::get_param_or_default(const std::initializer_list<const char *> &keys,
									const std::string &fallback) const
{
	for (const auto *key : keys)
	{
		auto it = _training_params.find(key);
		if (it != _training_params.end() && !it->second.empty())
		{
			return it->second;
		}
	}
	return fallback;
}

float NerlWorkerTorch::get_float_param(const std::initializer_list<const char *> &keys,
									float fallback) const
{
	const std::string value = get_param_or_default(keys, "");
	if (value.empty())
	{
		return fallback;
	}
	try
	{
		return std::stof(value);
	}
	catch (const std::exception &)
	{
		LogWarning << "Unable to parse float training param value '" << value << "'" << std::endl;
		return fallback;
	}
}

int NerlWorkerTorch::get_int_param(const std::initializer_list<const char *> &keys,
							   int fallback) const
{
	const std::string value = get_param_or_default(keys, "");
	if (value.empty())
	{
		return fallback;
	}
	try
	{
		return std::stoi(value);
	}
	catch (const std::exception &)
	{
		LogWarning << "Unable to parse integer training param value '" << value << "'" << std::endl;
		return fallback;
	}
}

bool NerlWorkerTorch::get_bool_param(const std::initializer_list<const char *> &keys,
							   bool fallback) const
{
	const std::string raw_value = get_param_or_default(keys, "");
	if (raw_value.empty())
	{
		return fallback;
	}
	const std::string value = to_lower_copy(raw_value);
	if (value == "true" || value == "1" || value == "yes" || value == "on")
	{
		return true;
	}
	if (value == "false" || value == "0" || value == "no" || value == "off")
	{
		return false;
	}
	LogWarning << "Unable to parse boolean training param value '" << raw_value << "'" << std::endl;
	return fallback;
}

void NerlWorkerTorch::maybe_randomize_module_weights()
{
	if (!_randomize_weights_on_load || _weights_randomized)
	{
		return;
	}
	if (!_has_script_module)
	{
		LogWarning << "Torch worker requested to randomize weights but no script module is loaded" << std::endl;
		return;
	}

	try
	{
		torch::NoGradGuard guard;
		size_t randomized_params = 0;
		for (auto named_param : _script_module.named_parameters(/*recurse=*/true))
		{
			torch::Tensor tensor = named_param.value;
			if (!tensor.defined())
			{
				continue;
			}
			tensor.normal_(0.0, 0.02);
			++randomized_params;
		}
		_weights_randomized = randomized_params > 0;
		LogInfo << "Torch worker randomized " << randomized_params << " parameters via w_init_rand" << std::endl;
	}
	catch (const std::exception &ex)
	{
		LogWarning << "Torch worker failed to randomize weights: " << ex.what() << std::endl;
	}
}

std::string NerlWorkerTorch::to_lower_copy(std::string value)
{
	std::transform(value.begin(), value.end(), value.begin(), [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
	return value;
}

} // namespace nerlnet