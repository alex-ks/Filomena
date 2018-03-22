import * as React from "react"
import * as Bootstrap from "reactstrap"

import "src/style.css"

interface IFsInputProps 
{ 
    onInput(sourceCode: string): void;
}

interface IFsInputState
{
    source: string;
}

export class FsInput extends React.Component<IFsInputProps, IFsInputState>
{
    constructor(props: IFsInputProps)
    {
        super(props);
        let sampleCode = [
            "module A",
            "let a = 1 + 1",
            "let b = a * 2"
        ].join("\n");
        this.state = { source: sampleCode };
    }

    handleChange = (event: React.ChangeEvent<HTMLInputElement>) => 
    {
        this.setState({ source: event.target.value });
    }

    handleCompile = (event: React.FormEvent<HTMLButtonElement>) =>
    {
        this.props.onInput(this.state.source);
    }

    render()
    {
        return (
                <Bootstrap.Form>
                    <div>
                        <Bootstrap.FormGroup>
                            <Bootstrap.Label 
                                for="source"
                                className="codeLabel">
                                Enter your source code:
                            </Bootstrap.Label>
                            <Bootstrap.Input 
                                type="textarea" 
                                id="source"
                                className="codeBox"
                                value={this.state.source}
                                onChange={this.handleChange} />
                            <Bootstrap.Button 
                                className="submitButton"
                                onClick={this.handleCompile}>
                                Compile
                            </Bootstrap.Button>
                        </Bootstrap.FormGroup>
                    </div>
                </Bootstrap.Form>
        );
    }
}
