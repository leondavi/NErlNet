#pragma once

#include <Logger.h>
#include <nifpp.h>

#include "../common/bridgeController.h"
#include "NerlWorkerTorch.h"

namespace nerlnet::torchbridge
{

inline ERL_NIF_TERM new_nerlworker_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	enum
	{
		ARG_MODEL_ID,
		ARG_DISTRIBUTED_SYSTEM_TYPE,
		ARG_DISTRIBUTED_SYSTEM_ARGS,
		ARG_TRAIN_PARAMS
	};

	try
	{
		unsigned long model_id;
		std::string distributed_system_type_str;
		std::string distributed_system_args_str;
		std::map<std::string, std::string> train_params;

		nifpp::get_throws(env, argv[ARG_MODEL_ID], model_id);
		nifpp::get_throws(env, argv[ARG_DISTRIBUTED_SYSTEM_TYPE], distributed_system_type_str);
		nifpp::get_throws(env, argv[ARG_DISTRIBUTED_SYSTEM_ARGS], distributed_system_args_str);
		nifpp::get_throws(env, argv[ARG_TRAIN_PARAMS], train_params);

		LogInfo << "Torch new_nerlworker_nif invoked for model " << model_id
				<< " with " << train_params.size() << " train params" << std::endl;

		int distributed_system_type = 0;
		try
		{
			distributed_system_type = std::stoi(distributed_system_type_str);
		}
		catch (const std::exception &)
		{
			distributed_system_type = 0;
		}

		auto worker = std::make_shared<NerlWorkerTorch>(distributed_system_type,
														  distributed_system_args_str,
														  train_params);

		BridgeController &controller = BridgeController::GetInstance();
		controller.setData(worker, model_id);

		return nifpp::make(env, nifpp::str_atom("ok"));
	}
	catch (const std::exception &ex)
	{
		LogError << "Torch new_nerlworker_nif threw exception: " << ex.what() << std::endl;
		return enif_make_badarg(env);
	}
	catch (...)
	{
		LogError << "Torch new_nerlworker_nif failed with unknown exception" << std::endl;
		return enif_make_badarg(env);
	}
}

inline ERL_NIF_TERM test_nerlworker_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	// Same implementation as new_nerlworker for now to keep planner tooling happy.
	return new_nerlworker_nif(env, argc, argv);
}

inline ERL_NIF_TERM remove_nerlworker_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	enum
	{
		ARG_MODEL_ID
	};

	unsigned long model_id;
	nifpp::get_throws(env, argv[ARG_MODEL_ID], model_id);
	BridgeController &controller = BridgeController::GetInstance();
	controller.deleteModel(model_id);
	return nifpp::make(env, nifpp::str_atom("ok"));
}

inline ERL_NIF_TERM update_nerlworker_train_params_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	LogWarning << "update_nerlworker_train_params_nif for Torch is not implemented yet" << std::endl;
	return nifpp::make(env, nifpp::str_atom("ok"));
}

} // namespace nerlnet::torchbridge

