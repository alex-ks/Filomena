import * as React from "react"
import * as Bootstrap from "reactstrap"

import { FsInput } from "src/components/FsInput"
import { ResultBox } from "src/components/ResultBox"

interface IAppProps
{
    compilerUrl: string;
}

interface IAppState
{
    compiledCode: string;
    hasErrors: boolean;
}

export class App extends React.Component<IAppProps, IAppState>
{
    constructor(props: IAppProps) {
        super(props);
        this.state = { compiledCode: "", hasErrors: false };
    }

    compileCode = (sourceCode: string) =>
    {
        try
        {
            this.setState({ hasErrors: false });
            let requester = new XMLHttpRequest();
            requester.open('POST', this.props.compilerUrl, false);
            requester.setRequestHeader("Content-Type", "application/json");
            requester.send(JSON.stringify({ source: sourceCode }));
            let compiled = requester.responseText;
            let hasErrors = requester.status != 200;
            this.setState({ compiledCode: compiled, hasErrors: hasErrors });
        }
        catch (_)
        {
            this.setState({ hasErrors: true });
        }
    }

    render() : React.ReactNode
    {
        return (
            <div className="container">
                <Bootstrap.Alert
                    color="danger"
                    style={{ margin: "1em 0em" }}
                    hidden={!this.state.hasErrors}>
                    Something went wrong...
                </Bootstrap.Alert>
                <div className="row">
                    <div className="col-md-6">
                        <FsInput onInput={this.compileCode} />
                    </div>
                    <div className="col-md-6">
                        <ResultBox result={this.state.compiledCode} />
                    </div>
                </div>
            </div>
        );
    }
}