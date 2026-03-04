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
#include <tuple>
#include <unordered_map>
#include <utility>

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

	/** Runs a full training step (forward + MSE loss + backward + optimizer step) on @p batch. */
	TorchTensor train_batch(const TorchTensor &batch);

	/**
	 * Runs the forward + loss + backward pass for a single microbatch without immediately
	 * applying the optimizer step. Gradients are accumulated across microbatches.
	 * Call optimizer_barrier() after the last microbatch to flush the accumulated gradients.
	 *
	 * @param batch         Raw flat batch tensor (features + labels interleaved per sample).
	 * @param microbatch_id Zero-based index of this microbatch within the current batch.
	 * @return              Loss scalar tensor for this microbatch.
	 */
	TorchTensor train_microbatch(const TorchTensor &batch, long microbatch_id);

	/**
	 * Commits accumulated microbatch gradients with a single optimizer step.
	 * In pipeline-parallel mode the step is deferred until all cached stage contexts have
	 * been consumed by their corresponding backward passes. If the maximum defer threshold
	 * (_optimizer_barrier_max_defers) is reached, stale contexts are discarded and the
	 * gradient state is reset to avoid dead-lock.
	 * Safe to call even when no deferred gradients are pending.
	 */
	void optimizer_barrier();

	/** Runs inference on @p batch and returns the model prediction tensor. */
	TorchTensor predict_batch(const TorchTensor &batch);

	/**
	 * Pipeline stage 0 training forward pass.
	 * Splits the raw flat @p batch tensor into inputs and labels, zeroes gradients on the
	 * first microbatch, runs the stage-local layers, caches the stage context (input +
	 * output tensors) for the subsequent backward pass, and returns the detached activation
	 * tensor together with the labels so they can be forwarded to the next pipeline stage.
	 *
	 * @param batch         Raw flat batch tensor (features + labels interleaved per sample).
	 * @param batch_id      Identifier of the current batch (used to key the context cache).
	 * @param microbatch_id Identifier of the current microbatch within the batch.
	 * @return              {stage_output_activation, labels} — both detached and cloned.
	 */
	std::tuple<TorchTensor, TorchTensor> pipeline_stage0_forward(const TorchTensor &batch, long batch_id, long microbatch_id);

	/**
	 * Intermediate pipeline stage training forward pass.
	 * Accepts an incoming activation tensor from the preceding stage, runs the local
	 * layers, caches the stage context for backward, and passes the output activation
	 * and labels unchanged to the next stage.
	 *
	 * @param activation    Detached activation received from the previous stage.
	 * @param labels        Label tensor forwarded from stage 0 (passed through unchanged).
	 * @param batch_id      Batch identifier for context cache keying.
	 * @param microbatch_id Microbatch identifier for context cache keying.
	 * @return              {stage_output_activation, labels} — both detached and cloned.
	 */
	std::tuple<TorchTensor, TorchTensor> pipeline_stage_forward(const TorchTensor &activation, const TorchTensor &labels, long batch_id, long microbatch_id);

	/**
	 * Last pipeline stage combined forward + backward pass (training).
	 * Runs the local layers on the incoming activation, computes MSE loss against the
	 * provided labels, triggers backward(), and returns the scalar loss together with
	 * the gradient w.r.t. the stage input so it can be propagated back to the preceding
	 * stage via pipeline_stage_backward().
	 *
	 * @param activation    Detached activation received from the previous stage.
	 * @param labels        Ground-truth labels for loss computation.
	 * @param batch_id      Batch identifier for context tracking.
	 * @param microbatch_id Microbatch identifier for context tracking.
	 * @return              {loss_scalar, grad_input} — loss and stage-input gradient, both detached.
	 */
	std::tuple<TorchTensor, TorchTensor> pipeline_stage_last_forward_backward(const TorchTensor &activation, const TorchTensor &labels, long batch_id, long microbatch_id);

	/**
	 * Intermediate (non-last) pipeline stage backward pass.
	 * Retrieves the saved forward context for {batch_id, microbatch_id}, backpropagates
	 * the received downstream gradient through the local layers via autograd::backward(),
	 * increments the deferred microbatch counter, and returns the gradient w.r.t. this
	 * stage's input for propagation to the preceding stage.
	 *
	 * @param grad_output   Gradient tensor received from the downstream stage.
	 * @param batch_id      Batch identifier matching a previously cached context.
	 * @param microbatch_id Microbatch identifier matching a previously cached context.
	 * @return              Gradient w.r.t. this stage's input, detached and cloned.
	 * @throws std::runtime_error if no cached context exists for the given keys.
	 */
	TorchTensor pipeline_stage_backward(const TorchTensor &grad_output, long batch_id, long microbatch_id);

	/**
	 * Pipeline stage 0 prediction (inference) forward pass.
	 * Prepares the input tensor (feature extraction / shape normalization), runs the
	 * stage-local layers in eval mode, and returns the detached output activation for
	 * forwarding to the next pipeline stage. Falls back to predict_batch() if the
	 * pipeline partition is not enabled.
	 *
	 * @param batch Raw batch tensor (may contain labels beyond the feature span).
	 * @return      Detached output activation cloned for transport to the next stage.
	 */
	TorchTensor pipeline_predict_stage0_forward(const TorchTensor &batch);

	/**
	 * Intermediate or last pipeline stage prediction (inference) forward pass.
	 * Runs the local layers in eval mode on the received activation tensor and returns
	 * the detached output for forwarding to the next stage (or as the final prediction).
	 * Falls back to predict_batch() if the pipeline partition is not enabled.
	 *
	 * @param activation Activation tensor received from the preceding pipeline stage.
	 * @return           Detached output activation cloned for transport.
	 */
	TorchTensor pipeline_predict_stage_forward(const TorchTensor &activation);

	/** Returns the loss tensor from the most recent training step. */
	TorchTensor last_loss() const { return _last_loss; }

	enum class LossFunctionType { MSE, CrossEntropy, L1, Huber };

private:
	static LossFunctionType parse_loss_function(const std::string &name);
	TorchTensor compute_loss(const TorchTensor &prediction, const TorchTensor &target) const;
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

	/**
	 * Holds the tensors saved during a pipeline stage forward pass so that the
	 * corresponding backward pass can retrieve them. The context is keyed by
	 * {batch_id, microbatch_id} and stored in _pipeline_stage_contexts.
	 */
	struct PipelineStageContext
	{
		/** Stage input tensor (requires_grad=true) saved before the forward pass. */
		TorchTensor stage_input;
		/** Stage output tensor produced by the forward pass, held for autograd. */
		TorchTensor stage_output;
	};

	void load_script_module();
	TorchTensor forward_or_clone(const TorchTensor &input, bool training_mode);
	TorchTensor ensure_training_dtype(const TorchTensor &batch) const;
	void initialize_training_config();
	void initialize_optimizer();
	void initialize_batch_layout();
	TrainingSlices split_training_batch(const TorchTensor &prepared) const;

	/**
	 * Extracts the feature (input) columns from a prepared batch tensor for prediction.
	 * Handles two cases: (a) the batch contains both features and labels (full sample span)
	 * — splits it and returns only the input slice; (b) the batch contains only features
	 * (input span only) — reshapes directly. Falls back to the raw tensor if neither span
	 * divides the element count evenly.
	 *
	 * @param prepared Contiguous batch tensor in training dtype.
	 * @return         Input-only tensor reshaped to the configured input_shape.
	 */
	TorchTensor prepare_predict_inputs(const TorchTensor &prepared) const;
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

	/**
	 * Discovers child layers from the loaded TorchScript module, partitions them evenly
	 * across pipeline stages (remainder layers are distributed to the first stages), and
	 * initialises _pipeline_layers, _pipeline_layer_names, _pipeline_stage_start_idx, and
	 * _pipeline_stage_end_idx. Sets _pipeline_enabled = true on success. Prefers the
	 * explicit `layers` attribute if present; falls back to named_children().
	 */
	void initialize_pipeline_partition();

	/**
	 * Runs the layers assigned to this pipeline stage sequentially on @p stage_input.
	 * Automatically flattens the input before linear (2-D weight) layers when the input
	 * has more than 2 dimensions. Falls back to forward_or_clone() if the pipeline
	 * partition is not enabled.
	 *
	 * @param stage_input    Input activation tensor for this stage.
	 * @param training_mode  If true, sets layers to train mode; otherwise eval mode.
	 * @param microbatch_id  Used only for logging (shape info is printed for microbatch 0).
	 * @return               Output activation after the last local layer.
	 * @throws std::runtime_error if any layer returns a non-tensor IValue.
	 */
	TorchTensor run_pipeline_stage_layers(const TorchTensor &stage_input, bool training_mode, long microbatch_id);

	/**
	 * Heuristic: returns true when @p layer_module exposes a 2-D `weight` parameter,
	 * indicating it is a fully-connected (linear) layer that likely requires a flattened
	 * input. Only inspects top-level (non-recursive) named parameters.
	 */
	static bool layer_requires_flatten(const torch::jit::script::Module &layer_module);

	/** Debug helper: formats tensor dimensions as "[d0,d1,...,dN]" or "undefined". */
	static std::string tensor_shape_to_string(const TorchTensor &tensor);

	/** Debug helper: returns the scalar-type name of @p tensor (e.g. "Float") or "undefined". */
	static std::string tensor_dtype_to_string(const TorchTensor &tensor);

	/** Debug helper: returns a human-readable type string for a torch::jit::IValue. */
	static std::string ivalue_type_to_string(const torch::jit::IValue &value);

	/**
	 * Returns the display name for the layer at @p idx.
	 * Falls back to "layer_N" when the name entry is absent or empty.
	 */
	std::string pipeline_layer_name(size_t idx) const;

	/**
	 * Generates the string key used to look up a PipelineStageContext in
	 * _pipeline_stage_contexts. Format: "<batch_id>:<microbatch_id>".
	 */
	std::string pipeline_context_key(long batch_id, long microbatch_id) const;

	/**
	 * Saves a PipelineStageContext (stage_input + stage_output) for the given
	 * {batch_id, microbatch_id} pair so it can be retrieved during the backward pass.
	 * Any pre-existing entry for the same key is overwritten.
	 */
	void cache_pipeline_stage_context(long batch_id, long microbatch_id, const TorchTensor &stage_input, const TorchTensor &stage_output);

	/**
	 * Retrieves and removes the PipelineStageContext for {batch_id, microbatch_id}.
	 * @throws std::runtime_error if no context is found for the given key.
	 */
	PipelineStageContext pop_pipeline_stage_context(long batch_id, long microbatch_id);

	/** Removes all cached PipelineStageContext entries. */
	void clear_pipeline_stage_contexts();

	/**
	 * Collects unique, gradient-requiring parameters for the optimizer.
	 * When the pipeline partition is active, only parameters belonging to the layers
	 * assigned to this stage ([_pipeline_stage_start_idx, _pipeline_stage_end_idx)) are
	 * collected. Falls back to full-model parameter collection if stage-scoped discovery
	 * yields no tensors.
	 *
	 * @return {parameters, stage_scoped} where stage_scoped is true when the parameters
	 *         were successfully restricted to the local pipeline stage.
	 */
	std::pair<std::vector<torch::Tensor>, bool> collect_trainable_parameters_for_optimizer() const;

	/**
	 * Core training implementation shared by train_batch() and train_microbatch().
	 * Splits the batch, runs forward + loss + backward. When @p defer_optimizer_step
	 * is true, the optimizer step is omitted and _deferred_microbatch_count is incremented
	 * so optimizer_barrier() can commit the accumulated gradients later. When false, the
	 * optimizer step is applied immediately and deferred state is reset.
	 *
	 * @param batch               Raw flat batch tensor (features + labels per sample).
	 * @param defer_optimizer_step If true, accumulate gradients without stepping.
	 * @param microbatch_id        Microbatch index, used only for logging and error messages (-1 for non-microbatch calls).
	 * @return                    Loss scalar tensor (detached).
	 */
	TorchTensor train_batch_impl(const TorchTensor &batch, bool defer_optimizer_step, long microbatch_id);

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
	bool _has_deferred_gradients{false};
	int64_t _deferred_microbatch_count{0};
	int64_t _optimizer_barrier_defer_count{0};
	int64_t _optimizer_barrier_max_defers{100};
	int64_t _num_microbatches_for_loss_scale{1};
	LossFunctionType _loss_function{LossFunctionType::MSE};
	bool _pipeline_enabled{false};
	int64_t _pipeline_stage{0};
	int64_t _pipeline_world_size{1};
	size_t _pipeline_stage_start_idx{0};
	size_t _pipeline_stage_end_idx{0};
	std::vector<torch::jit::script::Module> _pipeline_layers;
	std::vector<std::string> _pipeline_layer_names;
	std::unordered_map<std::string, PipelineStageContext> _pipeline_stage_contexts;
};

} // namespace nerlnet
