using Filomena.Backend.Models;
using System;
using System.Collections.Generic;
using System.Text;
using System.Threading.Tasks;

namespace Filomena.Backend.ExecutorClient
{
    public interface IExecutorClient
    {
        Task<int> ExecuteAsync(ComputationGraph task);
        Task<int> ExecuteAsync(ComputationGraph task, decimal budget);
        Task<int> ExecuteAsync(ComputationGraph task, DateTime deadline);
        Task<int> ExecuteAsync(ComputationGraph task, DateTime deadline, decimal budget);

        Task UpdateSessionDetailsAsync(int sessionId, decimal newBudget);
        Task UpdateSessionDetailsAsync(int sessionId, DateTime newDeadline);
        Task UpdateSessionDetailsAsync(int sessionId, DateTime newDeadline, decimal newBudget);

        Task<ComputationGraphDiff> GetStatusAsync(int sessionId);

        Task CancelExecutionAsync(int sessionId);

        Task UpdateExternalOperation(int sessionId, int operationId, OperationStatus status, string result);
        Task<(OperationStatus status, string result)> GetOperationStatus(int sessionId, int operationId);
    }
}
