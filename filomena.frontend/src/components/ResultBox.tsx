import * as React from "react"
import * as Bootstrap from "reactstrap"

import "src/style.css"

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
                    <div>
                        <Bootstrap.FormGroup>
                            <Bootstrap.Label 
                                for="result"
                                className="codeLabel">
                                Compiled code:
                            </Bootstrap.Label>
                            <Bootstrap.Input 
                                type="textarea" 
                                id="result"
                                className="codeBox"
                                value={this.props.result} />
                        </Bootstrap.FormGroup>
                    </div>
                </Bootstrap.Form>
        );
    }
}