namespace Filomena.Backend.ResolverClient
open System.Threading.Tasks

type IResolver = 
    abstract member ResolveAsync: AtomId seq -> AtomContent seq Task
    abstract member GetContentAsync: AtomId -> AtomContent Task
    abstract member CreateAsync: AtomId seq -> AtomContent -> AtomId Task
    abstract member GetInfoAsync: AtomId -> AtomInfo Task
    abstract member ExistsAsync: AtomId -> bool Task