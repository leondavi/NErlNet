from stats import Stats
from typing import List
import pandas as pd
import numpy as np
import os
from collections import OrderedDict
from definitions import PHASE_PREDICTION_STR, PHASE_TRAINING_STR

class ExperimentSummary:
    def __init__(self, stats_list: List[Stats]):
        self.stats_list = stats_list
        assert stats_list, "Stats list cannot be empty"
        self.workers = self.stats_list[0].workers_list
        
    def summary_headers(self):
        headers_ml_comm = ["Batch Size", "Frequency", "Num Of Sources", "Batches/Second", "Min. Accuracy", "Avg. Accuracy", "Min. Precision", "Avg. Precision", "Min. F1-Score", "Avg. F1-Score", "WX % Dropped Training", "WX % Dropped Prediction", "WX # Dropped Training", "WX # Dropped Prediction", "WX Total Batches Training", "WX Total Batches Prediction"]
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

    def get_model_performance_aggregates(self, stats_obj):
        """
        Get min/avg accuracy, precision, and F1-score from model performance stats
        """
        try:
            # First we need confusion matrices to get model performance
            confusion_matrices_source, confusion_matrices_worker = stats_obj.get_confusion_matrices()
            model_perf_df = stats_obj.get_model_performence_stats(confusion_matrices_worker)
            
            if not model_perf_df.empty:
                accuracies = model_perf_df['Accuracy'].values
                precisions = model_perf_df['Precision'].values
                f1_scores = model_perf_df['F1'].values
                
                return {
                    'min_accuracy': np.min(accuracies) if len(accuracies) > 0 else 0,
                    'avg_accuracy': np.mean(accuracies) if len(accuracies) > 0 else 0,
                    'min_precision': np.min(precisions) if len(precisions) > 0 else 0,
                    'avg_precision': np.mean(precisions) if len(precisions) > 0 else 0,
                    'min_f1': np.min(f1_scores) if len(f1_scores) > 0 else 0,
                    'avg_f1': np.mean(f1_scores) if len(f1_scores) > 0 else 0,
                }
            else:
                return {
                    'min_accuracy': 0, 'avg_accuracy': 0,
                    'min_precision': 0, 'avg_precision': 0,
                    'min_f1': 0, 'avg_f1': 0,
                }
        except Exception as e:
            # Return zeros if model performance can't be calculated (e.g., for training phase)
            return {
                'min_accuracy': 0, 'avg_accuracy': 0,
                'min_precision': 0, 'avg_precision': 0,
                'min_f1': 0, 'avg_f1': 0,
            }

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
            
            for worker_name in self.workers:
                if debug:
                    print(f"\n  Processing worker: {worker_name}")
                
                # Map worker to client using the same logic as before
                client_perf = {}
                possible_client_names = [
                    worker_name,
                    f"c{worker_name}",
                    f"client{worker_name}",
                    f"c{worker_name.replace('w', '')}",
                    f"client{worker_name.replace('w', '')}"
                ]
                
                if debug:
                    print(f"    Trying client mappings: {possible_client_names}")
                
                for possible_name in possible_client_names:
                    if possible_name in perf_stats:
                        client_perf = perf_stats[possible_name]
                        if debug:
                            print(f"    ✓ Training mapping found: {worker_name} -> {possible_name}")
                            print(f"    Client perf data keys: {list(client_perf.keys())}")
                        break
                
                if not client_perf and len(perf_stats) == 1:
                    client_perf = list(perf_stats.values())[0]
                    if debug:
                        print(f"    ✓ Training: Using single available client data for worker {worker_name}")
                        print(f"    Client perf data keys: {list(client_perf.keys())}")
                
                if not client_perf:
                    if debug:
                        print(f"    ✗ No client performance data found for worker {worker_name}")
                
                # Collect performance data from this training phase
                if client_perf:
                    memory_ema = client_perf.get('memory_train_ema_usage', 0)
                    memory_peak = client_perf.get('memory_train_peak_usage', 0)
                    time_active = client_perf.get('time_train_active', 0)
                    time_total = client_perf.get('time_train_total', 0)
                    
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
                        if core_num not in aggregated[worker_name]['cpu_train_util_per_core_list']:
                            aggregated[worker_name]['cpu_train_util_per_core_list'][core_num] = []
                        aggregated[worker_name]['cpu_train_util_per_core_list'][core_num].append(util)
        
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
        
        # Model performance aggregates
        model_perf = self.get_model_performance_aggregates(stats_obj)
        row_data["Min. Accuracy"] = model_perf['min_accuracy']
        row_data["Avg. Accuracy"] = model_perf['avg_accuracy']
        row_data["Min. Precision"] = model_perf['min_precision']
        row_data["Avg. Precision"] = model_perf['avg_precision']
        row_data["Min. F1-Score"] = model_perf['min_f1']
        row_data["Avg. F1-Score"] = model_perf['avg_f1']
        
        # Get aggregated training statistics
        training_aggregates = self.get_training_aggregates(stats_obj, debug=debug)
        
        # Communication stats per worker
        comm_stats = stats_obj.get_communication_stats_workers()
        for worker_name in self.workers:
            worker_comm = comm_stats.get(worker_name, {})
            
            # Current prediction phase stats
            predict_received = worker_comm.get('batches_received_predict', 0)
            predict_dropped = worker_comm.get('batches_dropped_predict', 0)
            predict_total = predict_received + predict_dropped
            predict_drop_pct = (predict_dropped / predict_total * 100) if predict_total > 0 else 0
            
            # Training batch counts - get from current stats object (prediction phase)
            # instead of aggregating from training phases to avoid double counting
            current_worker_comm = comm_stats.get(worker_name, {})
            train_dropped_current = current_worker_comm.get('batches_dropped_train', 0)
            train_received_current = current_worker_comm.get('batches_received_train', 0)
            train_total_current = train_dropped_current + train_received_current
            train_drop_pct_current = (train_dropped_current / train_total_current * 100) if train_total_current > 0 else 0
            
            row_data[f"{worker_name} % Dropped Training"] = train_drop_pct_current
            row_data[f"{worker_name} % Dropped Prediction"] = predict_drop_pct
            row_data[f"{worker_name} # Dropped Training"] = train_dropped_current
            row_data[f"{worker_name} # Dropped Prediction"] = predict_dropped
            row_data[f"{worker_name} Total Batches Training"] = train_total_current
            row_data[f"{worker_name} Total Batches Prediction"] = predict_total
        
        # Performance stats per worker (from clients)
        perf_stats = stats_obj.get_performance_stats_clients()
        
        # Debug: print available clients to understand the mapping
        if debug:
            print(f"Available clients in performance stats: {list(perf_stats.keys())}")
            print(f"Workers list: {self.workers}")
        
        for worker_name in self.workers:
            # Try different possible mappings: worker_name, c+worker_name, client+worker_name
            client_perf = {}
            possible_client_names = [
                worker_name,
                f"c{worker_name}",
                f"client{worker_name}",
                f"c{worker_name.replace('w', '')}",  # if worker is 'w1', try 'c1'
                f"client{worker_name.replace('w', '')}"  # if worker is 'w1', try 'client1'
            ]
            
            for possible_name in possible_client_names:
                if possible_name in perf_stats:
                    client_perf = perf_stats[possible_name]
                    if debug:
                        print(f"Found mapping: {worker_name} -> {possible_name}")
                    break
            
            if not client_perf:
                # If no direct mapping found, use the first available client if there's only one
                if len(perf_stats) == 1:
                    client_perf = list(perf_stats.values())[0]
                    if debug:
                        print(f"Warning: Using single available client data for worker {worker_name}")
            
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
            
            # Current prediction phase stats
            row_data[f"{worker_name} Accumulated Time Train Active"] = accumulated_train_active
            row_data[f"{worker_name} Accumulated Time Train Total"] = accumulated_train_total
            row_data[f"{worker_name} Accumulated Time Predict Active"] = client_perf.get('time_predict_active', 0)
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
                print(f"  {worker_name} Accumulated Time Predict Active: {client_perf.get('time_predict_active', 0)}")
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
                row_data[f"{worker_name} CPU Predict Util Core {core_num}"] = cpu_predict_util.get(core_num, 0)
                
                if debug:
                    print(f"  {worker_name} CPU Train Util Core {core_num}: {avg_cpu_train_util}")
                    print(f"  {worker_name} CPU Predict Util Core {core_num}: {cpu_predict_util.get(core_num, 0)}")
        
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
