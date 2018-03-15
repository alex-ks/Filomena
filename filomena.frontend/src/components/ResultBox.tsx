import * as React from "react"
import * as Bootstrap from "reactstrap"

interface IResultBoxProps
{
    result: string;
}

interface IResultBoxState
{
    result: string;
}

export class ResultBox extends React.Component<IResultBoxProps>
{
    render()
    {
        return (
            <Bootstrap.Form>
                    <div className="row">
                        <div className="col-md-3"></div>
                        <div className="col-md-6">
                            <Bootstrap.FormGroup>
                                <Bootstrap.Label 
                                    for="result"
                                    style=
                                    {{ 
                                        margin: "1em 0em",
                                        fontWeight: "bold"
                                    }}>Compiled code:</Bootstrap.Label>
                                <Bootstrap.Input 
                                    type="textarea" 
                                    id="result"
                                    style=
                                    {{ 
                                        height: "15em" ,
                                        fontFamily: "monospace"
                                    }}
                                    value={this.props.result} />
                            </Bootstrap.FormGroup>
                        </div>
                    </div>
                </Bootstrap.Form>
        );
    }
}