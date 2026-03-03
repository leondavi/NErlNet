from stats import Stats
from typing import List
import pandas as pd
import numpy as np
import os
from collections import OrderedDict
from definitions import PHASE_PREDICTION_STR, PHASE_TRAINING_STR
from logger import LOG_WARNING

class ExperimentSummary:
    def __init__(self, stats_list: List[Stats]):
        self.stats_list = stats_list
        assert stats_list, "Stats list cannot be empty"
        self.workers = self.stats_list[0].workers_list
        
    def summary_headers(self):
        headers_ml_comm = ["Batch Size", "Frequency", "Num Of Sources", "Samples/Second", "Effective Samples/Second", "Min. Accuracy", "Avg. Accuracy", "Min. Precision", "Avg. Precision", "Min. Recall", "Avg. Recall", "Min. F1-Score", "Avg. F1-Score", "WX % Dropped Training", "WX % Dropped Prediction", "WX # Dropped Training", "WX # Dropped Prediction", "WX Total Batches Training", "WX Total Batches Prediction", "WX TP Collective Count", "WX TP Collective Latency (us)", "WX TP Avg Collective Latency (us)", "WX Skip Grant Accept Timeout", "WX Skip Payload Delivery Timeout", "WX Skip Completion Timeout", "WX Skip Phase Close Drain", "WX Stale Event After Skip", "WX NaN Loss Count"]
        headers_perf = ["WX Accumulated Time Train Active", "WX Accumulated Time Train Total", "WX Accumulated Time Predict Active", "WX Accumulated Time Predict Total", "WX Memory Train EMA Usage", "WX Memory Predict EMA Usage", "WX Memory Train Peak Usage", "WX Memory Predict Peak Usage", "WX Num Of Cores", "WX CPU Train Util Core X", "WX CPU Predict Util Core X"]
        all_headers = headers_ml_comm + headers_perf
        return all_headers
    
    def expand_headers_workers(self):
        """
        Expand headers to replace WX with worker names and Core X with specific core numbers
        """
        all_headers = self.summary_headers()
        new_headers_list = []
        
        # Get the number of cores from performance stats (assuming first stats object has client data)
        if self.stats_list:
            perf_stats = self.stats_list[0].get_performance_stats_clients()
            num_cores = 0
            if perf_stats:
                first_client = list(perf_stats.values())[0]
                num_cores = first_client.get('num_of_cores', 0)
        
        for header in all_headers:
            if "WX" in header:
                for worker_name in self.workers:
                    if "Core X" in header:
                        # Handle CPU utilization headers with specific cores
                        for core_num in range(num_cores):
                            worker_core_header = header.replace("WX", worker_name).replace("Core X", f"Core {core_num}")
                            new_headers_list.append(worker_core_header)
                    else:
                        # Regular worker-specific headers
                        worker_header = header.replace("WX", worker_name)
                        new_headers_list.append(worker_header)
            else:
                new_headers_list.append(header)
        return new_headers_list

    def calculate_samples_per_second(self, stats_obj):
        """
        Calculate samples per second based on frequency and batch size
        """
        freq = stats_obj.freq
        batch_size = stats_obj.batch_size
        return freq * batch_size if freq and batch_size else 0

    def _get_worker_parallel_map(self, stats_obj):
        net_comps = getattr(stats_obj, "net_comps", None)
        if net_comps and hasattr(net_comps, "get_worker_parallel_map"):
            try:
                return net_comps.get_worker_parallel_map() or {}
            except Exception:
                return {}
        return {}

    def _get_worker_to_client_map(self, stats_obj):
        net_comps = getattr(stats_obj, "net_comps", None)
        if not net_comps:
            return {}
        if hasattr(net_comps, "get_map_worker_to_client"):
            try:
                return net_comps.get_map_worker_to_client() or {}
            except Exception:
                return {}
        mapping = {}
        if hasattr(net_comps, "get_client_name_by_worker_name"):
            for worker_name in self.workers:
                try:
                    mapping[worker_name] = net_comps.get_client_name_by_worker_name(worker_name)
                except Exception:
                    continue
        return mapping

    @staticmethod
    def _safe_int(value, default=0):
        try:
            return int(value)
        except Exception:
            return default

    @staticmethod
    def _safe_float(value, default=0.0):
        try:
            return float(value)
        except Exception:
            return default

    def _compute_worker_batch_totals(self, stats_obj, comm_stats):
        worker_totals = {}
        for worker_name in self.workers:
            worker_comm = comm_stats.get(worker_name, {})
            train_received = self._safe_int(worker_comm.get("batches_received_train", 0), 0)
            train_dropped = self._safe_int(worker_comm.get("batches_dropped_train", 0), 0)
            predict_received = self._safe_int(worker_comm.get("batches_received_predict", 0), 0)
            predict_dropped = self._safe_int(worker_comm.get("batches_dropped_predict", 0), 0)
            train_sent = self._safe_int(worker_comm.get("batches_sent_train", 0), 0)
            predict_sent = self._safe_int(worker_comm.get("batches_sent_predict", 0), 0)
            train_completed = self._safe_int(worker_comm.get("batches_completed_train", 0), 0)
            predict_completed = self._safe_int(worker_comm.get("batches_completed_predict", 0), 0)
            worker_totals[worker_name] = {
                "train_received": train_received,
                "train_dropped": train_dropped,
                "predict_received": predict_received,
                "predict_dropped": predict_dropped,
                "train_sent": train_sent,
                "predict_sent": predict_sent,
                "train_completed": train_completed,
                "predict_completed": predict_completed,
                "train_total": max(train_received + train_dropped, train_sent, train_completed),
                "predict_total": max(predict_received + predict_dropped, predict_sent, predict_completed),
            }

        worker_parallel_map = self._get_worker_parallel_map(stats_obj)
        pipeline_workers = []
        for worker_name in self.workers:
            worker_parallel = worker_parallel_map.get(worker_name, {})
            if isinstance(worker_parallel, dict) and worker_parallel.get("pipelineStage") is not None:
                pipeline_workers.append(worker_name)

        if not pipeline_workers:
            phase_parallel_mode = "legacy"
            try:
                parallel_execution = stats_obj.experiment_phase.get_parallel_execution() or {}
                phase_parallel_mode = str(parallel_execution.get("mode", "legacy"))
            except Exception:
                phase_parallel_mode = "legacy"
            if phase_parallel_mode in ("pipeline", "pipeline_tensor"):
                pipeline_workers = list(self.workers)

        if pipeline_workers:
            max_train_total = max(worker_totals[w]["train_total"] for w in pipeline_workers)
            max_predict_total = max(worker_totals[w]["predict_total"] for w in pipeline_workers)
            for worker_name in pipeline_workers:
                if worker_totals[worker_name]["train_total"] == 0 and max_train_total > 0:
                    worker_totals[worker_name]["train_total"] = max_train_total
                if worker_totals[worker_name]["predict_total"] == 0 and max_predict_total > 0:
                    worker_totals[worker_name]["predict_total"] = max_predict_total

        return worker_totals

    def _replicate_worker_perf_payload(self, payload):
        """Replicate client-level performance payload to a worker without scaling.

        Wall-clock time, memory, CPU utilization, and GPU metrics are
        system/client-level measurements that are shared across workers.
        Scaling them by activity weight would incorrectly reduce their
        values (e.g. halving wall-clock time for 2 equal workers, making
        Effective Samples/Second 2x too high).  Instead we replicate the
        client payload unchanged to every worker on that client.

        Per-worker NIF compute times (time_train_active / time_predict_active)
        are available directly from worker comm stats and should be read from
        there rather than approximated by splitting the client aggregate.
        """
        if not payload:
            return {}
        return dict(payload)

    def _resolve_worker_perf_stats(self, stats_obj, perf_stats, worker_totals, debug=False):
        worker_perf = {worker_name: {} for worker_name in self.workers}
        if not perf_stats:
            return worker_perf

        # Preferred path: payload is already keyed by worker name.
        for worker_name in self.workers:
            payload = perf_stats.get(worker_name)
            if isinstance(payload, dict):
                worker_perf[worker_name] = dict(payload)

        unresolved = [worker_name for worker_name, payload in worker_perf.items() if not payload]
        if not unresolved:
            return worker_perf

        worker_to_client = self._get_worker_to_client_map(stats_obj)
        client_to_workers = OrderedDict()
        for worker_name in self.workers:
            client_name = worker_to_client.get(worker_name)
            if client_name:
                client_to_workers.setdefault(client_name, []).append(worker_name)

        # Replicate client-level payload to each worker on that client.
        # Client-level metrics (wall-clock time, memory, CPU%) are shared
        # measurements and must not be scaled/divided among workers.
        for client_name, client_workers in client_to_workers.items():
            payload = perf_stats.get(client_name)
            if not isinstance(payload, dict):
                continue
            for worker_name in client_workers:
                if worker_perf.get(worker_name):
                    continue
                worker_perf[worker_name] = self._replicate_worker_perf_payload(payload)
                if debug:
                    print(
                        f"Replicated client perf {client_name} -> {worker_name}"
                    )

        unresolved = [worker_name for worker_name, payload in worker_perf.items() if not payload]
        resolved_count = len(self.workers) - len(unresolved)
        if unresolved and len(perf_stats) == 1 and resolved_count == 0:
            only_payload = list(perf_stats.values())[0]
            if isinstance(only_payload, dict):
                for worker_name in self.workers:
                    worker_perf[worker_name] = self._replicate_worker_perf_payload(only_payload)
        return worker_perf

    def get_model_performance_aggregates(self, stats_obj):
        """
        Get min/avg accuracy, precision, and F1-score from model performance stats
        """
        zero_metrics = {
            'min_accuracy': 0, 'avg_accuracy': 0,
            'min_precision': 0, 'avg_precision': 0,
            'min_recall': 0, 'avg_recall': 0,
            'min_f1': 0, 'avg_f1': 0,
        }
        try:
            # First we need confusion matrices to get model performance
            confusion_matrices_source, confusion_matrices_worker = stats_obj.get_confusion_matrices()
            model_perf_df = stats_obj.get_model_performence_stats(confusion_matrices_worker)

            if model_perf_df is None or model_perf_df.empty:
                return zero_metrics

            metric_columns = ['Accuracy', 'Precision', 'Recall', 'F1']
            if not all(column in model_perf_df.columns for column in metric_columns):
                LOG_WARNING(
                    f"Model performance DataFrame is missing required columns {metric_columns}; "
                    f"available columns={list(model_perf_df.columns)}"
                )
                return zero_metrics

            metric_df = model_perf_df[metric_columns].apply(pd.to_numeric, errors='coerce')
            metric_df = metric_df.replace([np.inf, -np.inf], np.nan)

            # Ignore degenerate rows when confusion matrix support is missing.
            if all(col in model_perf_df.columns for col in ['TN', 'FP', 'FN', 'TP']):
                support_df = model_perf_df[['TN', 'FP', 'FN', 'TP']].apply(pd.to_numeric, errors='coerce')
                support_mask = support_df.fillna(0).sum(axis=1) > 0
                metric_df = metric_df[support_mask]

            metric_df = metric_df.dropna(how='any')
            if metric_df.empty:
                return zero_metrics

            accuracies = metric_df['Accuracy'].values
            precisions = metric_df['Precision'].values
            recalls = metric_df['Recall'].values
            f1_scores = metric_df['F1'].values

            return {
                'min_accuracy': np.min(accuracies),
                'avg_accuracy': np.mean(accuracies),
                'min_precision': np.min(precisions),
                'avg_precision': np.mean(precisions),
                'min_recall': np.min(recalls),
                'avg_recall': np.mean(recalls),
                'min_f1': np.min(f1_scores),
                'avg_f1': np.mean(f1_scores),
            }
        except Exception as e:
            LOG_WARNING(
                f"Failed to compute model performance aggregates for "
                f"experiment='{stats_obj.get_name()}', phase='{stats_obj.get_phase()}': {e}"
            )
            return zero_metrics

    def get_training_aggregates(self, prediction_stats_obj, debug=False):
        """
        Get aggregated training statistics from all training phase Stats objects that belong to the same experiment
        """
        prediction_name = prediction_stats_obj.get_name()
        training_stats_objects = []
        
        if debug:
            print(f"\n=== DEBUG: get_training_aggregates for experiment: {prediction_name} ===")
            print(f"Total stats objects in list: {len(self.stats_list)}")
            for i, obj in enumerate(self.stats_list):
                print(f"  Stats object {i}: name='{obj.get_name()}', phase='{obj.get_phase()}'")
        
        # Strategy 1: Try exact name match first
        for stats_obj in self.stats_list:
            if (stats_obj.get_phase() == PHASE_TRAINING_STR and 
                stats_obj.get_name() == prediction_name):
                training_stats_objects.append(stats_obj)
                if debug:
                    print(f"  → Found exact name match: {stats_obj.get_name()} - {stats_obj.get_phase()}")
        
        # Strategy 2: If no exact match, try to find training phases with similar base names
        if not training_stats_objects:
            if debug:
                print(f"No exact name matches found. Trying pattern matching...")
            
            # Extract base name patterns (remove common suffixes/prefixes)
            prediction_base = prediction_name.replace('prediction_', '').replace('_prediction', '').replace('pred_', '').replace('_pred', '')
            
            for stats_obj in self.stats_list:
                if stats_obj.get_phase() == PHASE_TRAINING_STR:
                    training_name = stats_obj.get_name()
                    training_base = training_name.replace('training_', '').replace('_training', '').replace('train_', '').replace('_train', '').replace('phase1', '').replace('phase2', '').replace('phase3', '').replace('_phase', '').replace('phase_', '')
                    
                    if debug:
                        print(f"  Comparing: prediction_base='{prediction_base}' vs training_base='{training_base}' (from '{training_name}')")
                    
                    # Check if they share the same base name or if training name contains prediction base
                    if (prediction_base and training_base and 
                        (prediction_base in training_base or training_base in prediction_base or
                         prediction_base == training_base)):
                        training_stats_objects.append(stats_obj)
                        if debug:
                            print(f"  → Found pattern match: {stats_obj.get_name()} - {stats_obj.get_phase()}")
        
        # Strategy 3: If still no matches, include ALL training phases (fallback)
        if not training_stats_objects:
            if debug:
                print(f"No pattern matches found. Using all training phases as fallback...")
            
            for stats_obj in self.stats_list:
                if stats_obj.get_phase() == PHASE_TRAINING_STR:
                    training_stats_objects.append(stats_obj)
                    if debug:
                        print(f"  → Using training phase: {stats_obj.get_name()} - {stats_obj.get_phase()}")
        
        if not training_stats_objects:
            if debug:
                print(f"No training phase Stats objects found for experiment: {prediction_name}")
            return {}
        
        if debug:
            print(f"Final result: Found {len(training_stats_objects)} training phase Stats objects for experiment: {prediction_name}")
            for obj in training_stats_objects:
                print(f"  → Will use: {obj.get_name()}")
        
        # Rest of the aggregation logic remains the same...
        # Aggregate training statistics across all training phase Stats objects
        aggregated = {}
        
        # Initialize aggregated data structure for each worker
        for worker_name in self.workers:
            aggregated[worker_name] = {
                'memory_train_ema_usage_list': [],
                'memory_train_peak_usage_list': [],
                'cpu_train_util_per_core_list': {},
                'time_train_active_list': [],
                'time_train_total_list': []
            }
        
        # Collect data from each training phase Stats object
        for idx, train_stats_obj in enumerate(training_stats_objects):
            if debug:
                print(f"\n--- Processing training Stats object {idx+1}/{len(training_stats_objects)} ---")
                print(f"Name: {train_stats_obj.get_name()}, Phase: {train_stats_obj.get_phase()}")
            
            # Get performance stats from this training phase
            try:
                perf_stats = train_stats_obj.get_performance_stats_clients()
                if debug:
                    print(f"Performance stats keys: {list(perf_stats.keys())}")
                    for key, value in perf_stats.items():
                        print(f"  Client '{key}': {list(value.keys()) if isinstance(value, dict) else 'Not a dict'}")
            except Exception as e:
                if debug:
                    print(f"Error getting performance stats: {e}")
                perf_stats = {}

            train_comm_stats = train_stats_obj.get_communication_stats_workers()
            train_worker_totals = self._compute_worker_batch_totals(train_stats_obj, train_comm_stats)
            worker_perf_stats = self._resolve_worker_perf_stats(
                train_stats_obj,
                perf_stats,
                train_worker_totals,
                debug=debug,
            )
            
            for worker_name in self.workers:
                if debug:
                    print(f"\n  Processing worker: {worker_name}")

                client_perf = worker_perf_stats.get(worker_name, {})
                worker_comm = train_comm_stats.get(worker_name, {})
                if not client_perf:
                    if debug:
                        print(f"    ✗ No client performance data found for worker {worker_name}")

                # Collect performance data from this training phase
                if client_perf or worker_comm:
                    memory_ema = client_perf.get('memory_train_ema_usage', 0) if client_perf else 0
                    memory_peak = client_perf.get('memory_train_peak_usage', 0) if client_perf else 0
                    # Per-worker NIF compute time from worker comm stats (actual per-worker
                    # value), not the client-level aggregate from perf stats.
                    time_active = self._safe_float(worker_comm.get('acc_time_training', 0))
                    time_total = client_perf.get('time_train_total', 0) if client_perf else 0
                    
                    aggregated[worker_name]['memory_train_ema_usage_list'].append(memory_ema)
                    aggregated[worker_name]['memory_train_peak_usage_list'].append(memory_peak)
                    aggregated[worker_name]['time_train_active_list'].append(time_active)
                    aggregated[worker_name]['time_train_total_list'].append(time_total)
                    
                    if debug:
                        print(f"    Performance data collected:")
                        print(f"      memory_train_ema_usage: {memory_ema}")
                        print(f"      memory_train_peak_usage: {memory_peak}")
                        print(f"      time_train_active: {time_active}")
                        print(f"      time_train_total: {time_total}")
                    
                    # CPU utilization per core from this training phase
                    cpu_train_util = client_perf.get('cpu_train_util_per_core', {})
                    if debug:
                        print(f"      cpu_train_util_per_core: {cpu_train_util}")
                    
                    for core_num, util in cpu_train_util.items():
                        core_idx = self._safe_int(core_num, 0)
                        if core_idx not in aggregated[worker_name]['cpu_train_util_per_core_list']:
                            aggregated[worker_name]['cpu_train_util_per_core_list'][core_idx] = []
                        aggregated[worker_name]['cpu_train_util_per_core_list'][core_idx].append(util)
        
        # Debug: Print final aggregated data
        if debug:
            print(f"\n=== FINAL AGGREGATED DATA ===")
            for worker_name in self.workers:
                print(f"Worker {worker_name}:")
                agg_data = aggregated[worker_name]
                print(f"  memory_train_ema_usage_list: {agg_data['memory_train_ema_usage_list']}")
                print(f"  memory_train_peak_usage_list: {agg_data['memory_train_peak_usage_list']}")
                print(f"  time_train_active_list: {agg_data['time_train_active_list']}")
                print(f"  time_train_total_list: {agg_data['time_train_total_list']}")
                print(f"  cpu_train_util_per_core_list: {agg_data['cpu_train_util_per_core_list']}")
        
        return aggregated

    def generate_summary_row(self, stats_obj, debug=False):
        """
        Generate a single row of summary data for one stats object
        """
        row_data = OrderedDict()

        # Basic experiment info
        row_data["Batch Size"] = stats_obj.batch_size
        row_data["Frequency"] = stats_obj.freq
        row_data["Num Of Sources"] = stats_obj.num_of_sources
        row_data["Samples/Second"] = self.calculate_samples_per_second(stats_obj)
        row_data["Effective Samples/Second"] = 0

        # Model performance aggregates
        model_perf = self.get_model_performance_aggregates(stats_obj)
        row_data["Min. Accuracy"] = model_perf['min_accuracy']
        row_data["Avg. Accuracy"] = model_perf['avg_accuracy']
        row_data["Min. Precision"] = model_perf['min_precision']
        row_data["Avg. Precision"] = model_perf['avg_precision']
        row_data["Min. Recall"] = model_perf['min_recall']
        row_data["Avg. Recall"] = model_perf['avg_recall']
        row_data["Min. F1-Score"] = model_perf['min_f1']
        row_data["Avg. F1-Score"] = model_perf['avg_f1']

        # Get aggregated training statistics
        training_aggregates = self.get_training_aggregates(stats_obj, debug=debug)

        # Communication stats per worker
        comm_stats = stats_obj.get_communication_stats_workers()
        worker_totals = self._compute_worker_batch_totals(stats_obj, comm_stats)
        for worker_name in self.workers:
            worker_comm = comm_stats.get(worker_name, {})
            worker_totals_dict = worker_totals.get(worker_name, {})

            # Current prediction phase stats
            predict_received = self._safe_int(worker_totals_dict.get('predict_received', 0), 0)
            predict_dropped = self._safe_int(worker_totals_dict.get('predict_dropped', 0), 0)
            predict_total = self._safe_int(worker_totals_dict.get('predict_total', 0), 0)
            predict_drop_pct = (predict_dropped / predict_total * 100) if predict_total > 0 else 0

            # Training batch counts - get from current stats object (prediction phase)
            # instead of aggregating from training phases to avoid double counting
            train_dropped_current = self._safe_int(worker_totals_dict.get('train_dropped', 0), 0)
            train_total_current = self._safe_int(worker_totals_dict.get('train_total', 0), 0)
            train_drop_pct_current = (train_dropped_current / train_total_current * 100) if train_total_current > 0 else 0

            row_data[f"{worker_name} % Dropped Training"] = train_drop_pct_current
            row_data[f"{worker_name} % Dropped Prediction"] = predict_drop_pct
            row_data[f"{worker_name} # Dropped Training"] = train_dropped_current
            row_data[f"{worker_name} # Dropped Prediction"] = predict_dropped
            row_data[f"{worker_name} Total Batches Training"] = train_total_current
            row_data[f"{worker_name} Total Batches Prediction"] = predict_total
            tp_collective_count = int(worker_comm.get('tp_collective_count', 0) or 0)
            tp_collective_latency_us = int(worker_comm.get('tp_collective_latency_us', 0) or 0)
            tp_collective_avg_latency_us = worker_comm.get('tp_collective_avg_latency_us')
            if tp_collective_avg_latency_us is None:
                tp_collective_avg_latency_us = (
                    float(tp_collective_latency_us) / float(tp_collective_count)
                    if tp_collective_count > 0
                    else 0.0
                )
            row_data[f"{worker_name} TP Collective Count"] = tp_collective_count
            row_data[f"{worker_name} TP Collective Latency (us)"] = tp_collective_latency_us
            row_data[f"{worker_name} TP Avg Collective Latency (us)"] = float(tp_collective_avg_latency_us)

            # Pipeline skip counters (accumulated in worker ETS across phases)
            row_data[f"{worker_name} Skip Grant Accept Timeout"] = int(worker_comm.get('skip_grant_accept_timeout', 0) or 0)
            row_data[f"{worker_name} Skip Payload Delivery Timeout"] = int(worker_comm.get('skip_payload_delivery_timeout', 0) or 0)
            row_data[f"{worker_name} Skip Completion Timeout"] = int(worker_comm.get('skip_completion_timeout', 0) or 0)
            row_data[f"{worker_name} Skip Phase Close Drain"] = int(worker_comm.get('skip_phase_close_drain', 0) or 0)
            row_data[f"{worker_name} Stale Event After Skip"] = int(worker_comm.get('stale_event_after_skip', 0) or 0)
            row_data[f"{worker_name} NaN Loss Count"] = int(worker_comm.get('nan_loss_count', 0) or 0)

        # Performance stats per worker (from clients)
        perf_stats = stats_obj.get_performance_stats_clients()
        worker_perf_stats = self._resolve_worker_perf_stats(
            stats_obj,
            perf_stats,
            worker_totals,
            debug=debug,
        )

        # Debug: print available clients to understand the mapping
        if debug:
            print(f"Available clients in performance stats: {list(perf_stats.keys())}")
            print(f"Workers list: {self.workers}")

        for worker_name in self.workers:
            worker_comm = comm_stats.get(worker_name, {})
            client_perf = worker_perf_stats.get(worker_name, {})

            # Per-worker NIF compute time from worker comm stats (actual per-worker
            # values from the worker ETS, not the scaled client-level aggregate).
            worker_predict_active = self._safe_float(worker_comm.get('acc_time_prediction', 0))

            # Aggregated training performance stats
            if worker_name in training_aggregates:
                train_agg = training_aggregates[worker_name]

                if debug:
                    print(f"\n=== APPLYING AGGREGATION RULES FOR {worker_name} ===")
                    print(f"Raw training data:")
                    print(f"  time_train_active_list: {train_agg['time_train_active_list']}")
                    print(f"  time_train_total_list: {train_agg['time_train_total_list']}")
                    print(f"  memory_train_ema_usage_list: {train_agg['memory_train_ema_usage_list']}")
                    print(f"  memory_train_peak_usage_list: {train_agg['memory_train_peak_usage_list']}")

                # Accumulated time (sum across all training phases)
                accumulated_train_active = sum(train_agg['time_train_active_list'])
                accumulated_train_total = sum(train_agg['time_train_total_list'])

                # Average EMA memory usage across training phases
                avg_memory_train_ema = (np.mean(train_agg['memory_train_ema_usage_list'])
                                       if train_agg['memory_train_ema_usage_list'] else 0)

                # Max peak memory usage across training phases
                max_memory_train_peak = (max(train_agg['memory_train_peak_usage_list'])
                                        if train_agg['memory_train_peak_usage_list'] else 0)

                if debug:
                    print(f"Applied aggregation rules:")
                    print(f"  accumulated_train_active (sum): {accumulated_train_active}")
                    print(f"  accumulated_train_total (sum): {accumulated_train_total}")
                    print(f"  avg_memory_train_ema (avg): {avg_memory_train_ema}")
                    print(f"  max_memory_train_peak (max): {max_memory_train_peak}")
            else:
                if debug:
                    print(f"\n=== NO TRAINING AGGREGATES FOUND FOR {worker_name} ===")
                accumulated_train_active = 0
                accumulated_train_total = 0
                avg_memory_train_ema = 0
                max_memory_train_peak = 0

            # Time Active: per-worker NIF compute time from worker comm stats
            # Time Total: client-level wall-clock time (replicated, not scaled)
            row_data[f"{worker_name} Accumulated Time Train Active"] = accumulated_train_active
            row_data[f"{worker_name} Accumulated Time Train Total"] = accumulated_train_total
            row_data[f"{worker_name} Accumulated Time Predict Active"] = worker_predict_active
            row_data[f"{worker_name} Accumulated Time Predict Total"] = client_perf.get('time_predict_total', 0)
            row_data[f"{worker_name} Memory Train EMA Usage"] = avg_memory_train_ema
            row_data[f"{worker_name} Memory Predict EMA Usage"] = client_perf.get('memory_predict_ema_usage', 0)
            row_data[f"{worker_name} Memory Train Peak Usage"] = max_memory_train_peak
            row_data[f"{worker_name} Memory Predict Peak Usage"] = client_perf.get('memory_predict_peak_usage', 0)
            row_data[f"{worker_name} Num Of Cores"] = client_perf.get('num_of_cores', 0)

            if debug:
                print(f"\n=== FINAL VALUES ASSIGNED TO CSV FOR {worker_name} ===")
                print(f"  {worker_name} Accumulated Time Train Active: {accumulated_train_active}")
                print(f"  {worker_name} Accumulated Time Train Total: {accumulated_train_total}")
                print(f"  {worker_name} Memory Train EMA Usage: {avg_memory_train_ema}")
                print(f"  {worker_name} Memory Train Peak Usage: {max_memory_train_peak}")
                print(f"  {worker_name} Accumulated Time Predict Active: {worker_predict_active}")
                print(f"  {worker_name} Memory Predict EMA Usage: {client_perf.get('memory_predict_ema_usage', 0)}")

            # CPU utilization per core
            cpu_predict_util = client_perf.get('cpu_predict_util_per_core', {})
            num_cores = client_perf.get('num_of_cores', 0)

            for core_num in range(num_cores):
                # Average CPU training utilization across all training phases
                if (worker_name in training_aggregates and
                    core_num in training_aggregates[worker_name]['cpu_train_util_per_core_list']):
                    avg_cpu_train_util = np.mean(training_aggregates[worker_name]['cpu_train_util_per_core_list'][core_num])
                else:
                    avg_cpu_train_util = 0

                row_data[f"{worker_name} CPU Train Util Core {core_num}"] = avg_cpu_train_util
                row_data[f"{worker_name} CPU Predict Util Core {core_num}"] = cpu_predict_util.get(
                    core_num, cpu_predict_util.get(str(core_num), 0)
                )

                if debug:
                    print(f"  {worker_name} CPU Train Util Core {core_num}: {avg_cpu_train_util}")
                    print(
                        f"  {worker_name} CPU Predict Util Core {core_num}: "
                        f"{cpu_predict_util.get(core_num, cpu_predict_util.get(str(core_num), 0))}"
                    )
        max_predict_batches = 0
        max_predict_time_us = 0.0
        for worker_name in self.workers:
            predict_batches = self._safe_int(
                worker_totals.get(worker_name, {}).get("predict_total", 0), 0
            )
            predict_time = self._safe_float(
                worker_perf_stats.get(worker_name, {}).get("time_predict_total", 0), 0.0
            )
            max_predict_batches = max(max_predict_batches, predict_batches)
            max_predict_time_us = max(max_predict_time_us, predict_time)
        if max_predict_batches > 0 and max_predict_time_us > 0:
            row_data["Effective Samples/Second"] = (
                float(max_predict_batches * stats_obj.batch_size)
                / (max_predict_time_us / 1_000_000.0)
            )

        return row_data

    def generate_summary_csv(self, output_path=None, debug=False, force_append=False):
        """
        Generate a comprehensive summary CSV from all stats objects
        Only includes prediction phase experiments
        
        Args:
            output_path: Path to save the CSV file
            debug: Enable debug printing
            force_append: If True, always append new rows without checking for duplicates.
                         If False (default), replace existing experiments with same name.
        """
        import os
        
        summary_rows = []
        
        for stats_obj in self.stats_list:
            # Only include prediction phases
            if stats_obj.get_phase() == PHASE_PREDICTION_STR:
                row_data = self.generate_summary_row(stats_obj, debug=debug)
                # Add identifiers for this experiment
                row_data_with_id = OrderedDict()
                row_data_with_id["Phase"] = stats_obj.get_phase()
                row_data_with_id["Experiment"] = f"{stats_obj.get_name()}"
                row_data_with_id.update(row_data)
                summary_rows.append(row_data_with_id)
        
        # Create DataFrame for new data
        new_summary_df = pd.DataFrame(summary_rows)
        
        # Save to CSV if path provided
        if output_path:
            if os.path.exists(output_path):
                # File exists, append new rows
                try:
                    existing_df = pd.read_csv(output_path)
                    
                    if debug:
                        print(f"Existing file has {len(existing_df)} rows")
                        print(f"Adding {len(new_summary_df)} new rows")
                        print(f"New experiments being added: {new_summary_df['Experiment'].tolist()}")
                        if not existing_df.empty:
                            print(f"Existing experiments: {existing_df['Experiment'].tolist()}")
                        print(f"Force append mode: {force_append}")
                    
                    if force_append:
                        # Always append without checking for duplicates
                        combined_df = pd.concat([existing_df, new_summary_df], ignore_index=True)
                        print(f"Force appended {len(new_summary_df)} experiment(s) to existing file")
                    else:
                        # Check for exact duplicate experiments and handle them
                        if not existing_df.empty and not new_summary_df.empty:
                            new_experiment_names = set(new_summary_df['Experiment'].tolist())
                            existing_experiment_names = set(existing_df['Experiment'].tolist())
                            
                            # Find which experiments are duplicates
                            duplicate_experiments = new_experiment_names.intersection(existing_experiment_names)
                            truly_new_experiments = new_experiment_names - existing_experiment_names
                            
                            if debug:
                                print(f"Duplicate experiments to replace: {list(duplicate_experiments)}")
                                print(f"Truly new experiments to add: {list(truly_new_experiments)}")
                            
                            if duplicate_experiments:
                                # Remove only the exact duplicates from existing data
                                existing_df_filtered = existing_df[~existing_df['Experiment'].isin(duplicate_experiments)]
                                print(f"Replacing {len(duplicate_experiments)} existing experiment(s) with updated data")
                            else:
                                existing_df_filtered = existing_df
                                
                            if truly_new_experiments:
                                print(f"Adding {len(truly_new_experiments)} new experiment(s)")
                            
                            # Combine filtered existing data with new data
                            combined_df = pd.concat([existing_df_filtered, new_summary_df], ignore_index=True)
                        else:
                            # If either DataFrame is empty, just concatenate
                            combined_df = pd.concat([existing_df, new_summary_df], ignore_index=True)
                    
                    # Save the combined DataFrame
                    combined_df.to_csv(output_path, index=False)
                    
                    if debug:
                        print(f"Final file has {len(combined_df)} rows")
                        print(f"Experiments in final file: {combined_df['Experiment'].tolist()}")
                    
                    print(f"Updated file: {output_path} (now contains {len(combined_df)} total experiments)")
                    
                except Exception as e:
                    print(f"Warning: Could not read existing file {output_path}. Creating new file. Error: {e}")
                    new_summary_df.to_csv(output_path, index=False)
                    print(f"New summary file created: {output_path}")
            else:
                # File doesn't exist, create new file
                new_summary_df.to_csv(output_path, index=False)
                print(f"New summary file created with {len(new_summary_df)} experiment(s): {output_path}")
        
        return new_summary_df

    def print_summary_stats(self):
        """
        Print basic summary statistics
        """
        print("Experiment Summary Statistics:")
        print(f"Number of experiments: {len(self.stats_list)}")
        print(f"Workers: {self.workers}")
        
        for i, stats_obj in enumerate(self.stats_list):
            print(f"\nExperiment {i+1}: {stats_obj.get_name()} ({stats_obj.get_phase()})")
            print(f"  Batch Size: {stats_obj.batch_size}")
            print(f"  Frequency: {stats_obj.freq}")
            print(f"  Number of Sources: {stats_obj.num_of_sources}")
            print(f"  Samples/Second: {self.calculate_samples_per_second(stats_obj)}")
