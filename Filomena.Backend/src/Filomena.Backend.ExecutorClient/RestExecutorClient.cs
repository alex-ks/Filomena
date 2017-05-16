using System;
using System.Collections.Generic;
using System.Text;
using Filomena.Backend.Data.Models;
using System.Net.Http;
using System.Threading.Tasks;
using Newtonsoft.Json;

namespace Filomena.Backend.ExecutorClient
{
    public class RestExecutorClient : IExecutorClient, IDisposable
    {
        private HttpClient httpClient = new HttpClient();
        private string host;
        private int port;

        public RestExecutorClient(string host, int port)
        {
            this.host = host;
            this.port = port;
        }
        private async Task<int> ExecuteAsync<T>(T bodyObject)
        {
            var builder = new UriBuilder()
            {
                Host = host,
                Port = port,
                Path = "/session"
            };

            var requestContent = new StringContent(JsonConvert.SerializeObject(bodyObject));

            using (var responce = await httpClient.PostAsync(builder.Uri, requestContent))
            using (var content = responce.Content)
            {
                return JsonConvert.DeserializeObject<dynamic>(await content.ReadAsStringAsync()).sessionId;
            }
        }

        public Task<int> ExecuteAsync(ComputationGraph task)
        {
            return ExecuteAsync(new
            {
                computationGraph = task
            });
        }

        public Task<int> ExecuteAsync(ComputationGraph task, decimal budget)
        {
            return ExecuteAsync(new
            {
                computationGraph = task,
                budget = budget
            });
        }

        public Task<int> ExecuteAsync(ComputationGraph task, DateTime deadline)
        {
            return ExecuteAsync(new
            {
                computationGraph = task,
                deadline = deadline
            });
        }

        public Task<int> ExecuteAsync(ComputationGraph task, DateTime deadline, decimal budget)
        {
            return ExecuteAsync(new
            {
                computationGraph = task,
                budget = budget,
                deadline = deadline
            });
        }

        public async Task<ComputationGraphDiff> GetStatusAsync(int sessionId)
        {
            var builder = new UriBuilder()
            {
                Host = host,
                Port = port,
                Path = $"/session/{sessionId}"
            };

            using (var responce = await httpClient.GetAsync(builder.Uri))
            using (var content = responce.Content)
            {
                return JsonConvert.DeserializeObject<ComputationGraphDiff>(await content.ReadAsStringAsync());
            }
        }

        private async Task UpdateSessionDetailsAsync<T>(int sessionId, T bodyObject)
        {
            var builder = new UriBuilder()
            {
                Host = host,
                Port = port,
                Path = $"/session/{sessionId}"
            };

            var requestContent = new StringContent(JsonConvert.SerializeObject(bodyObject));

            using (var responce = await httpClient.PutAsync(builder.Uri, requestContent))
            using (var content = responce.Content)
            {
                responce.EnsureSuccessStatusCode();
            }
        }

        public Task UpdateSessionDetailsAsync(int sessionId, decimal newBudget)
        {
            return UpdateSessionDetailsAsync(sessionId, new
            {
                budget = newBudget
            });
        }

        public Task UpdateSessionDetailsAsync(int sessionId, DateTime newDeadline)
        {
            return UpdateSessionDetailsAsync(sessionId, new
            {
                deadline = newDeadline
            });
        }

        public Task UpdateSessionDetailsAsync(int sessionId, DateTime newDeadline, decimal newBudget)
        {
            return UpdateSessionDetailsAsync(sessionId, new
            {
                deadline = newDeadline,
                budget = newBudget
            });
        }

        public async Task CancelExecutionAsync(int sessionId)
        {
            var builder = new UriBuilder()
            {
                Host = host,
                Port = port,
                Path = $"/session/{sessionId}"
            };

            using (var responce = await httpClient.DeleteAsync(builder.Uri))
            using (var content = responce.Content)
            {
                responce.EnsureSuccessStatusCode();
            }
        }

        public void Dispose()
        {
            httpClient.Dispose();
        }

        public async Task UpdateExternalOperation(int sessionId, int operationId, OperationStatus status, string result)
        {
            var builder = new UriBuilder()
            {
                Host = host,
                Port = port,
                Path = $"/session/{sessionId}/operation/{operationId}"
            };

            var requestContent = new StringContent(JsonConvert.SerializeObject(new
            {
                status = status.ToString(),
                result = result
            }));

            using (var responce = await httpClient.PutAsync(builder.Uri, requestContent))
            using (var content = responce.Content)
            {
                responce.EnsureSuccessStatusCode();
            }
        }

        public async Task<(OperationStatus status, string result)> GetOperationStatus(int sessionId, int operationId)
        {
            var builder = new UriBuilder()
            {
                Host = host,
                Port = port,
                Path = $"/session/{sessionId}/operation/{operationId}"
            };

            using (var responce = await httpClient.GetAsync(builder.Uri))
            using (var content = responce.Content)
            {
                return JsonConvert.DeserializeObject<(OperationStatus status, string result)>(await content.ReadAsStringAsync());
            }
        }
    }
}
