namespace Filomena.Backend.Parsing

type ('t, 'e) Maybe = Ok of 't | Failed of 'e
