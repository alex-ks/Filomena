function CompileCode(code: string): string
{
    let requester = new XMLHttpRequest();
    requester.open('POST', 'http://localhost:8080', false);
    requester.send(JSON.stringify({ source: code }));
    return requester.responseText;
}

function GetTextareaValue(id: string)
{
    return (<HTMLTextAreaElement>document.getElementById(id)).value;
}

function SetTextareaValue(id: string, text: string)
{
    (<HTMLInputElement>document.getElementById(id)).value = text;
}

function GetInputValue(id: string)
{
    return (<HTMLInputElement>document.getElementById(id)).value;
}

function HandleCompile()
{
    let text = GetTextareaValue("source");
    let compiled = CompileCode(text);
    SetTextareaValue("compiled", compiled);
}