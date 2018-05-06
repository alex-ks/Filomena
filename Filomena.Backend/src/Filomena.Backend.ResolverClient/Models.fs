namespace Filomena.Backend.ResolverClient

type AtomId = { Kind: string
                Name: string
                Version: string }

type AtomContent = { Kind: string
                     Name: string
                     Version: string
                     Content: byte[] }

type AtomInfo = { Kind: string
                  Name: string
                  Version: string
                  Dependencies: AtomId seq }
