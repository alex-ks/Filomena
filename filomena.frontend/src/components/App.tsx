import * as React from "react"

import { FsInput } from "src/components/FsInput"
import { ResultBox } from "src/components/ResultBox"

interface IAppProps
{
    compilerUrl: string;
}

interface IAppState
{
    compiledCode: string;
}

export class App extends React.Component<IAppProps, IAppState>
{
    constructor(props: IAppProps) {
        super(props);
        this.state = { compiledCode: "" };
    }

    compileCode = (sourceCode: string) =>
    {
        let requester = new XMLHttpRequest();
        requester.open('POST', this.props.compilerUrl, false);
        requester.setRequestHeader("Content-Type", "application/json");
        requester.send(JSON.stringify({ source: sourceCode }));
        let compiled = requester.responseText;
        this.setState({ compiledCode: compiled });
    }

    render() : React.ReactNode
    {
        return (
            <div className="container">
                <FsInput onInput={this.compileCode} />
                <ResultBox result={this.state.compiledCode} />
            </div>
        );
    }
}