// TODO: Remove when real classes are implemented
namespace Filomena.Backend.Parsing

type Platform () = class end

module Platform = 
    let getModuleDefinition (platform: Platform) (moduleName: string) = 
        do ignore platform
        do ignore moduleName
        ""
    