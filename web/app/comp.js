// @flow

var React = require('react')

export class FormSection extends React.Component {
  render():React.Element {

    var contentStyle = {
      padding: 15,
    }

    var headerStyle = {
      backgroundColor: '#F2F2F2',
      padding: 15,
    }

    var mainStyle = {
      border: 'solid 1px #D8D8D8',
      marginBottom: 20
    }

    return <div>
      <div style={mainStyle}>
        <div style={headerStyle}>{this.props.title}</div>
        <div style={contentStyle}>{this.props.children}</div>
      </div>
    </div>
  }
}

export class DisabledButton extends React.Component {
  render():React.Element {

    if (this.props.disabled) {
      var text = "Disabled"
      var className = "secondary"
    }

    else {
      var text = "Active"
      var className = "success"
    }

    return <div>
      <button className={className} onClick={this.props.onClick}>{text}</button>
    </div>
  }


  //toggleActive() {
    //var source = this.state.source
    //source.disabled = !source.disabled
    //this.setState({source: source})
  //}

}
