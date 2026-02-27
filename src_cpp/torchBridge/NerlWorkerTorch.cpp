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
		initialize_pipeline_partition();

		LogInfo << "Torch worker configured with lr=" << _configured_learning_rate
				<< ", epochs=" << _configured_epochs
				<< ", optimizer=" << _optimizer_name
			<< ", randomize_on_load=" << _randomize_weights_on_load
				<< ", has_layout=" << _has_batch_layout
				<< ", has_optimizer=" << _has_optimizer
				<< ", pipeline_enabled=" << _pipeline_enabled
				<< ", pipeline_stage=" << _pipeline_stage
				<< ", pipeline_world_size=" << _pipeline_world_size << std::endl;
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

TorchTensor NerlWorkerTorch::prepare_predict_inputs(const TorchTensor &prepared) const
{
	if (!_has_batch_layout)
	{
		return prepared;
	}

	TorchTensor contiguous = prepared.contiguous();
	TorchTensor flattened = contiguous.reshape({contiguous.numel()});
	if (flattened.numel() <= 0)
	{
		return contiguous;
	}

	const int64_t sample_span = _batch_layout.required_elements();
	if (sample_span > 0 && flattened.numel() % sample_span == 0)
	{
		TrainingSlices slices = split_training_batch(contiguous);
		return slices.inputs;
	}

	const int64_t input_span = _batch_layout.input_sample_span;
	if (input_span > 0 && flattened.numel() % input_span == 0)
	{
		const int64_t sample_count = flattened.numel() / input_span;
		TorchTensor input_matrix = flattened.reshape({sample_count, input_span});
		std::vector<int64_t> input_view_shape = _batch_layout.input_shape;
		if (!input_view_shape.empty())
		{
			input_view_shape[0] = sample_count;
		}
		else
		{
			input_view_shape.push_back(sample_count);
		}

		const int64_t expected_input_elements = count_elements(input_view_shape);
		if (expected_input_elements == flattened.numel())
		{
			return input_matrix.reshape(torch::IntArrayRef(input_view_shape));
		}
		return input_matrix;
	}

	LogWarning << "Torch worker prediction batch does not match configured spans. numel="
			   << flattened.numel() << " sample_span=" << sample_span
			   << " input_span=" << input_span << "; using raw batch tensor" << std::endl;
	return contiguous;
}

void NerlWorkerTorch::initialize_pipeline_partition()
{
	_pipeline_enabled = false;
	_pipeline_layers.clear();
	_pipeline_layer_names.clear();
	_pipeline_stage_contexts.clear();
	_pipeline_stage_start_idx = 0;
	_pipeline_stage_end_idx = 0;

	_pipeline_stage = std::max<int64_t>(0, get_int_param({"pipeline_stage"}, 0));
	_pipeline_world_size = std::max<int64_t>(1, get_int_param({"pipeline_world_size"}, 1));
	if (_pipeline_world_size <= 1 || !_has_script_module)
	{
		return;
	}

	std::vector<torch::jit::script::Module> discovered_layers;
	std::vector<std::string> discovered_layer_names;
	try
	{
		if (_script_module.hasattr("layers"))
		{
			torch::jit::IValue layers_value = _script_module.attr("layers");
			if (layers_value.isModule())
			{
				torch::jit::script::Module layer_container = layers_value.toModule();
				for (const auto &named_layer : layer_container.named_children())
				{
					discovered_layers.push_back(named_layer.value);
					discovered_layer_names.push_back(named_layer.name);
				}
			}
		}
	}
	catch (const std::exception &ex)
	{
		LogWarning << "Torch worker failed to inspect script module 'layers' attribute: " << ex.what() << std::endl;
	}

	if (discovered_layers.empty())
	{
		for (const auto &named_layer : _script_module.named_children())
		{
			discovered_layers.push_back(named_layer.value);
			discovered_layer_names.push_back(named_layer.name);
		}
	}

	if (discovered_layers.empty())
	{
		LogWarning << "Torch pipeline partition unavailable: model exposes no child modules" << std::endl;
		return;
	}

	if (_pipeline_stage >= _pipeline_world_size)
	{
		LogWarning << "Torch pipeline stage index " << _pipeline_stage
				   << " is invalid for world size " << _pipeline_world_size
				   << "; forcing stage 0" << std::endl;
		_pipeline_stage = 0;
	}

	const size_t total_layers = discovered_layers.size();
	const size_t world_size = static_cast<size_t>(_pipeline_world_size);
	const size_t stage = static_cast<size_t>(_pipeline_stage);
	const size_t base = total_layers / world_size;
	const size_t remainder = total_layers % world_size;

	size_t start_idx = 0;
	for (size_t idx = 0; idx < stage; ++idx)
	{
		start_idx += base + (idx < remainder ? 1 : 0);
	}
	const size_t stage_layer_count = base + (stage < remainder ? 1 : 0);
	if (stage_layer_count == 0)
	{
		LogWarning << "Torch pipeline stage " << _pipeline_stage
				   << " would own zero layers out of " << total_layers
				   << " (world size " << _pipeline_world_size << ")" << std::endl;
		return;
	}

	_pipeline_layers = std::move(discovered_layers);
	_pipeline_layer_names = std::move(discovered_layer_names);
	if (_pipeline_layer_names.size() != _pipeline_layers.size())
	{
		_pipeline_layer_names.resize(_pipeline_layers.size());
	}
	for (size_t idx = 0; idx < _pipeline_layer_names.size(); ++idx)
	{
		if (_pipeline_layer_names[idx].empty())
		{
			_pipeline_layer_names[idx] = "layer_" + std::to_string(idx);
		}
	}
	_pipeline_stage_start_idx = start_idx;
	_pipeline_stage_end_idx = start_idx + stage_layer_count;
	_pipeline_enabled = true;

	std::ostringstream stage_layers_desc;
	for (size_t idx = _pipeline_stage_start_idx; idx < _pipeline_stage_end_idx; ++idx)
	{
		if (idx > _pipeline_stage_start_idx)
		{
			stage_layers_desc << ", ";
		}
		stage_layers_desc << idx << ":" << pipeline_layer_name(idx);
	}

	LogInfo << "Torch pipeline partition stage=" << _pipeline_stage
			<< "/" << _pipeline_world_size
			<< " layer_range=[" << _pipeline_stage_start_idx
			<< "," << _pipeline_stage_end_idx << ")"
			<< " layers={" << stage_layers_desc.str() << "}" << std::endl;
}

bool NerlWorkerTorch::layer_requires_flatten(const torch::jit::script::Module &layer_module)
{
	for (const auto &named_param : layer_module.named_parameters(/*recurse=*/false))
	{
		if (named_param.name == "weight" && named_param.value.defined() && named_param.value.dim() == 2)
		{
			return true;
		}
	}
	return false;
}

std::string NerlWorkerTorch::tensor_shape_to_string(const TorchTensor &tensor)
{
	if (!tensor.defined())
	{
		return "undefined";
	}
	std::ostringstream oss;
	oss << "[";
	for (int64_t idx = 0; idx < tensor.dim(); ++idx)
	{
		if (idx > 0)
		{
			oss << ",";
		}
		oss << tensor.size(idx);
	}
	oss << "]";
	return oss.str();
}

std::string NerlWorkerTorch::tensor_dtype_to_string(const TorchTensor &tensor)
{
	if (!tensor.defined())
	{
		return "undefined";
	}
	return std::string(c10::toString(tensor.scalar_type()));
}

std::string NerlWorkerTorch::ivalue_type_to_string(const torch::jit::IValue &value)
{
	if (value.isNone())
	{
		return "None";
	}
	if (value.isTensor())
	{
		return "Tensor";
	}
	try
	{
		const c10::TypePtr type_ptr = value.type();
		if (type_ptr)
		{
			return type_ptr->str();
		}
	}
	catch (...)
	{
	}
	return "UnknownIValue";
}

std::string NerlWorkerTorch::pipeline_layer_name(size_t idx) const
{
	if (idx < _pipeline_layer_names.size() && !_pipeline_layer_names[idx].empty())
	{
		return _pipeline_layer_names[idx];
	}
	return "layer_" + std::to_string(idx);
}

TorchTensor NerlWorkerTorch::run_pipeline_stage_layers(const TorchTensor &stage_input, bool training_mode, long microbatch_id)
{
	if (!_pipeline_enabled)
	{
		return forward_or_clone(stage_input, training_mode);
	}

	TorchTensor output = stage_input;
	for (size_t idx = _pipeline_stage_start_idx; idx < _pipeline_stage_end_idx; ++idx)
	{
		if (idx >= _pipeline_layers.size())
		{
			throw std::runtime_error("Torch pipeline stage layer index out of range");
		}
		torch::jit::script::Module &layer = _pipeline_layers[idx];
		const std::string layer_name = pipeline_layer_name(idx);
		if (layer_requires_flatten(layer) && output.dim() > 2)
		{
			output = output.flatten(1);
		}
		if (microbatch_id == 0)
		{
			LogInfo << "Torch pipeline layer begin stage=" << _pipeline_stage
					<< " microbatch=" << microbatch_id
					<< " layer_idx=" << idx
					<< " layer_name=" << layer_name
					<< " mode=" << (training_mode ? "train" : "predict")
					<< " input_shape=" << tensor_shape_to_string(output)
					<< " input_dtype=" << tensor_dtype_to_string(output) << std::endl;
		}
		if (training_mode)
		{
			layer.train();
		}
		else
		{
			layer.eval();
		}

		std::vector<torch::jit::IValue> inputs;
		inputs.emplace_back(output);
		torch::jit::IValue out_val = layer.forward(inputs);
		if (!out_val.isTensor())
		{
			std::ostringstream err;
			err << "Torch pipeline stage layer returned non-tensor output"
				<< " stage=" << _pipeline_stage
				<< " microbatch=" << microbatch_id
				<< " layer_idx=" << idx
				<< " layer_name=" << layer_name
				<< " input_shape=" << tensor_shape_to_string(output)
				<< " input_dtype=" << tensor_dtype_to_string(output)
				<< " output_ivalue_type=" << ivalue_type_to_string(out_val);
			LogError << err.str() << std::endl;
			throw std::runtime_error(err.str());
		}
		output = out_val.toTensor();
		if (microbatch_id == 0)
		{
			LogInfo << "Torch pipeline layer end stage=" << _pipeline_stage
					<< " microbatch=" << microbatch_id
					<< " layer_idx=" << idx
					<< " layer_name=" << layer_name
					<< " output_shape=" << tensor_shape_to_string(output)
					<< " output_dtype=" << tensor_dtype_to_string(output) << std::endl;
		}
	}
	return output;
}

void NerlWorkerTorch::cache_pipeline_stage_context(long microbatch_id, const TorchTensor &stage_input, const TorchTensor &stage_output)
{
	PipelineStageContext context;
	context.stage_input = stage_input;
	context.stage_output = stage_output;
	_pipeline_stage_contexts[microbatch_id] = context;
}

NerlWorkerTorch::PipelineStageContext NerlWorkerTorch::pop_pipeline_stage_context(long microbatch_id)
{
	auto it = _pipeline_stage_contexts.find(microbatch_id);
	if (it == _pipeline_stage_contexts.end())
	{
		throw std::runtime_error("Missing pipeline stage context for microbatch " + std::to_string(microbatch_id));
	}
	PipelineStageContext context = it->second;
	_pipeline_stage_contexts.erase(it);
	return context;
}

void NerlWorkerTorch::clear_pipeline_stage_contexts()
{
	_pipeline_stage_contexts.clear();
}

std::tuple<TorchTensor, TorchTensor> NerlWorkerTorch::pipeline_stage0_forward(const TorchTensor &batch, long microbatch_id)
{
	if (!_pipeline_enabled)
	{
		throw std::runtime_error("pipeline_stage0_forward called but pipeline partition is not enabled");
	}

	if (!_has_optimizer)
	{
		throw std::runtime_error("pipeline_stage0_forward called without optimizer initialization");
	}

	TorchTensor prepared = ensure_training_dtype(batch);
	TrainingSlices slices = split_training_batch(prepared);
	if (!_has_deferred_gradients)
	{
		_optimizer->zero_grad();
		_has_deferred_gradients = true;
		_deferred_microbatch_count = 0;
		clear_pipeline_stage_contexts();
	}

	TorchTensor stage_input = slices.inputs.detach().set_requires_grad(true);
	TorchTensor stage_output = run_pipeline_stage_layers(stage_input, true, microbatch_id);
	cache_pipeline_stage_context(microbatch_id, stage_input, stage_output);
	_last_prediction = stage_output.detach();
	return {stage_output.detach().clone(), slices.labels.detach().clone()};
}

std::tuple<TorchTensor, TorchTensor> NerlWorkerTorch::pipeline_stage_forward(
	const TorchTensor &activation,
	const TorchTensor &labels,
	long microbatch_id
)
{
	if (!_pipeline_enabled)
	{
		throw std::runtime_error("pipeline_stage_forward called but pipeline partition is not enabled");
	}

	if (!_has_optimizer)
	{
		throw std::runtime_error("pipeline_stage_forward called without optimizer initialization");
	}

	if (!_has_deferred_gradients)
	{
		_optimizer->zero_grad();
		_has_deferred_gradients = true;
		_deferred_microbatch_count = 0;
		clear_pipeline_stage_contexts();
	}

	TorchTensor stage_input = ensure_training_dtype(activation).detach().set_requires_grad(true);
	TorchTensor stage_output = run_pipeline_stage_layers(stage_input, true, microbatch_id);
	cache_pipeline_stage_context(microbatch_id, stage_input, stage_output);
	_last_prediction = stage_output.detach();
	return {stage_output.detach().clone(), ensure_training_dtype(labels).detach().clone()};
}

std::tuple<TorchTensor, TorchTensor> NerlWorkerTorch::pipeline_stage_last_forward_backward(
	const TorchTensor &activation,
	const TorchTensor &labels,
	long microbatch_id
)
{
	if (!_pipeline_enabled)
	{
		throw std::runtime_error("pipeline_stage_last_forward_backward called but pipeline partition is not enabled");
	}

	if (!_has_optimizer)
	{
		throw std::runtime_error("pipeline_stage_last_forward_backward called without optimizer initialization");
	}

	if (!_has_deferred_gradients)
	{
		_optimizer->zero_grad();
		_has_deferred_gradients = true;
		_deferred_microbatch_count = 0;
		clear_pipeline_stage_contexts();
	}

	TorchTensor stage_input = ensure_training_dtype(activation).detach().set_requires_grad(true);
	TorchTensor stage_output = run_pipeline_stage_layers(stage_input, true, microbatch_id);
	TorchTensor target_labels = ensure_training_dtype(labels);
	if (stage_output.scalar_type() != target_labels.scalar_type())
	{
		stage_output = stage_output.to(target_labels.scalar_type());
	}
	if (stage_output.numel() != target_labels.numel())
	{
		std::ostringstream oss;
		oss << "Pipeline last stage prediction element mismatch. pred=" << stage_output.numel()
			<< " labels=" << target_labels.numel() << " microbatch=" << microbatch_id;
		throw std::runtime_error(oss.str());
	}
	if (stage_output.sizes() != target_labels.sizes())
	{
		stage_output = stage_output.reshape(target_labels.sizes());
	}

	TorchTensor loss = torch::mse_loss(stage_output, target_labels);
	loss.backward();
	TorchTensor grad_input = stage_input.grad();
	if (!grad_input.defined())
	{
		grad_input = torch::zeros_like(stage_input);
	}
	++_deferred_microbatch_count;
	_last_loss = loss.detach();
	_last_prediction = stage_output.detach();
	return {_last_loss.clone(), grad_input.detach().clone()};
}

TorchTensor NerlWorkerTorch::pipeline_stage_backward(const TorchTensor &grad_output, long microbatch_id)
{
	if (!_pipeline_enabled)
	{
		throw std::runtime_error("pipeline_stage_backward called but pipeline partition is not enabled");
	}

	PipelineStageContext context = pop_pipeline_stage_context(microbatch_id);
	TorchTensor local_grad = ensure_training_dtype(grad_output);
	if (local_grad.scalar_type() != context.stage_output.scalar_type())
	{
		local_grad = local_grad.to(context.stage_output.scalar_type());
	}
	if (local_grad.numel() != context.stage_output.numel())
	{
		std::ostringstream oss;
		oss << "Pipeline backward gradient element mismatch. grad=" << local_grad.numel()
			<< " output=" << context.stage_output.numel() << " microbatch=" << microbatch_id;
		throw std::runtime_error(oss.str());
	}
	if (local_grad.sizes() != context.stage_output.sizes())
	{
		local_grad = local_grad.reshape(context.stage_output.sizes());
	}

	torch::autograd::backward({context.stage_output}, {local_grad});
	TorchTensor grad_input = context.stage_input.grad();
	if (!grad_input.defined())
	{
		grad_input = torch::zeros_like(context.stage_input);
	}
	++_deferred_microbatch_count;
	return grad_input.detach().clone();
}

TorchTensor NerlWorkerTorch::pipeline_predict_stage0_forward(const TorchTensor &batch)
{
	if (!_pipeline_enabled)
	{
		return predict_batch(batch);
	}

	TorchTensor prepared = ensure_training_dtype(batch);
	TorchTensor stage_input = prepare_predict_inputs(prepared);
	TorchTensor stage_output = run_pipeline_stage_layers(stage_input, false, -1);
	return stage_output.detach().clone();
}

TorchTensor NerlWorkerTorch::pipeline_predict_stage_forward(const TorchTensor &activation)
{
	if (!_pipeline_enabled)
	{
		return predict_batch(activation);
	}

	TorchTensor stage_input = ensure_training_dtype(activation);
	TorchTensor stage_output = run_pipeline_stage_layers(stage_input, false, -1);
	return stage_output.detach().clone();
}

TorchTensor NerlWorkerTorch::train_batch_impl(const TorchTensor &batch, bool defer_optimizer_step, long microbatch_id)
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
		if (defer_optimizer_step)
		{
			if (!_has_deferred_gradients)
			{
				_optimizer->zero_grad();
				_has_deferred_gradients = true;
				_deferred_microbatch_count = 0;
			}
		}
		else
		{
			_optimizer->zero_grad();
		}

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
		if (defer_optimizer_step)
		{
			++_deferred_microbatch_count;
		}
		else
		{
			_optimizer->step();
			_optimizer->zero_grad();
			_has_deferred_gradients = false;
			_deferred_microbatch_count = 0;
		}

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
		if (!defer_optimizer_step)
		{
			_has_deferred_gradients = false;
			_deferred_microbatch_count = 0;
		}
		else
		{
			LogWarning << "Torch deferred microbatch " << microbatch_id << " failed; barrier may flush partial gradients" << std::endl;
		}
		return fallback;
	}
}

TorchTensor NerlWorkerTorch::train_batch(const TorchTensor &batch)
{
	return train_batch_impl(batch, false, -1);
}

TorchTensor NerlWorkerTorch::train_microbatch(const TorchTensor &batch, long microbatch_id)
{
	return train_batch_impl(batch, true, microbatch_id);
}

void NerlWorkerTorch::optimizer_barrier()
{
	if (!_has_optimizer || !_has_script_module)
	{
		_has_deferred_gradients = false;
		_deferred_microbatch_count = 0;
		clear_pipeline_stage_contexts();
		return;
	}

	if (_has_deferred_gradients && _deferred_microbatch_count > 0)
	{
		try
		{
			_optimizer->step();
			_optimizer->zero_grad();
		}
		catch (const std::exception &ex)
		{
			LogWarning << "Torch optimizer_barrier failed: " << ex.what() << std::endl;
		}
	}

	_has_deferred_gradients = false;
	_deferred_microbatch_count = 0;
	clear_pipeline_stage_contexts();
}

TorchTensor NerlWorkerTorch::predict_batch(const TorchTensor &batch)
{
	TorchTensor prepared = ensure_training_dtype(batch);
	TorchTensor predict_input = prepare_predict_inputs(prepared);
	TorchTensor prediction = forward_or_clone(predict_input, false);
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
