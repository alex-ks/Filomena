import * as React from "react"
import * as Bootstrap from "reactstrap"

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
                    <div className="row">
                        <div className="col-md-3"></div>
                        <div className="col-md-6">
                            <Bootstrap.FormGroup>
                                <Bootstrap.Label 
                                    for="source"
                                    style=
                                    {{ 
                                        margin: "1em 0em",
                                        fontWeight: "bold"
                                    }}>Enter your source code:</Bootstrap.Label>
                                <Bootstrap.Input 
                                    type="textarea" 
                                    id="source"
                                    style=
                                    {{ 
                                        height: "15em" ,
                                        fontFamily: "monospace"
                                    }}
                                    value={this.state.source}
                                    onChange={this.handleChange} />
                                <Bootstrap.Button 
                                    style={{ margin: "1em 0em" }}
                                    onClick={this.handleCompile}>Compile</Bootstrap.Button>
                            </Bootstrap.FormGroup>
                        </div>
                    </div>
                </Bootstrap.Form>
        );
    }
}
