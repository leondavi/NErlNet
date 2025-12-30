#pragma once

#include <torch/torch.h>
#include <torch/script.h>
#include <torch/optim.h>
#include <Logger.h>
#include <string>
#include <map>
#include <initializer_list>
#include <memory>
#include <vector>

#include "../common/nerlWorker.h"
#include "worker_definitions_ag.h"
#include "nerltensorTorchDefs.h"

namespace nerlnet
{

class NerlWorkerTorch : public NerlWorker
{
public:
    using TrainingParams = std::map<std::string, std::string>;

	NerlWorkerTorch(int distributed_system_type,
					std::string distributed_system_args_str,
					TrainingParams training_params);

	TorchTensor train_batch(const TorchTensor &batch);
	TorchTensor predict_batch(const TorchTensor &batch);
	TorchTensor last_loss() const { return _last_loss; }

private:
	struct BatchLayout
	{
		std::vector<int64_t> input_shape;
		std::vector<int64_t> label_shape;
		int64_t labels_offset{0};
		int64_t input_elem_count{0};
		int64_t label_elem_count{0};
		int64_t expected_batch_size{0};
		int64_t input_sample_span{0};
		int64_t label_sample_span{0};
		int64_t sample_elem_span{0};
		int64_t required_elements() const;
		bool valid() const;
	};

	struct TrainingSlices
	{
		TorchTensor inputs;
		TorchTensor labels;
	};

	void load_script_module();
	TorchTensor forward_or_clone(const TorchTensor &input, bool training_mode);
	TorchTensor ensure_training_dtype(const TorchTensor &batch) const;
	void initialize_training_config();
	void initialize_optimizer();
	void initialize_batch_layout();
	TrainingSlices split_training_batch(const TorchTensor &prepared) const;
	std::vector<int64_t> parse_shape_param(const std::initializer_list<const char *> &keys) const;
	static std::vector<int64_t> parse_shape_spec(const std::string &text);
	static int64_t count_elements(const std::vector<int64_t> &dims);
	static int64_t count_suffix_elements(const std::vector<int64_t> &dims);
	int64_t resolve_labels_offset(const std::string &value, int64_t default_offset) const;
	std::string get_param_or_default(const std::initializer_list<const char *> &keys,
									const std::string &fallback) const;
	float get_float_param(const std::initializer_list<const char *> &keys,
						  float fallback) const;
	int get_int_param(const std::initializer_list<const char *> &keys,
					 int fallback) const;
	bool get_bool_param(const std::initializer_list<const char *> &keys,
					   bool fallback) const;
	void maybe_randomize_module_weights();
	static std::string to_lower_copy(std::string value);

	TorchTensor _last_prediction;
	TorchTensor _last_loss;
	torch::jit::script::Module _script_module;
	bool _has_script_module{false};
	std::string _model_path;
	TrainingParams _training_params;
	float _configured_learning_rate{0.0F};
	int _configured_epochs{0};
	std::string _optimizer_name;
	std::vector<torch::Tensor> _trainable_parameters;
	BatchLayout _batch_layout;
	bool _has_batch_layout{false};
	bool _randomize_weights_on_load{false};
	bool _weights_randomized{false};
	std::unique_ptr<torch::optim::Optimizer> _optimizer;
	bool _has_optimizer{false};
};

} // namespace nerlnet