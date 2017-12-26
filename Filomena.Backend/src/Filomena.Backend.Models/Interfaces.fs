namespace Filomena.Backend.Models

type IFsSourceProvider = 
    abstract member GetModuleSources: string -> string

module FsSourceProvider = 
    let getModuleSources (provider: IFsSourceProvider) moduleName = provider.GetModuleSources moduleName