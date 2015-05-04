
//data ImportSettings = 
  //MenuSettings {
    //menuBase :: URL,
    //menuOpen :: Text,
    //menuClose ::  Text
  //} |
  //TOCSettings {
    //tocSelector :: Text
  //}
  //deriving (Show, Eq, Generic)

var React = require('react')
var {Menu, TOC} = require('../helpers')

export class MenuSettings extends React.Component {

  onChange(field) {
    return (e) => {
      var settings = this.props.settings
      settings[field] = e.target.value
      this.props.onUpdate(settings)
    }
  }

  render() {
    var settings = this.props.settings

    return <div>
      <label placeholder="twigserial.wordpress.com/?cat=">Base URL</label>
      <input type="text" value={settings.menuBase} onChange={this.onChange("menuBase")} />

      <div className="row">
        <div className="small-6 columns">
          <label placeholder="#chap_select">Open Selector</label>
          <input type="text" value={settings.menuOpen} onChange={this.onChange("menuOpen")} />
        </div>

        <div className="small-6 columns">
          <label placeholder="select">Close Selector</label>
          <input type="text" value={settings.menuClose} onChange={this.onChange("menuClose")} />
        </div>
      </div>
    </div>
  }
}

export class TOCSettings extends React.Component {

  onChange() {
    return (e) => {
      var settings = this.props.settings
      settings.tocSelector = e.target.value
      this.props.onUpdate(settings)
    }
  }
  render() {
    var settings = this.props.settings
    return <div>
      <label placeholder="#toc">Selector</label>
      <input type="text" value={settings.tocSelector} onChange={this.onChange()} />
    </div>
  }
}


export class ImportSettings extends React.Component {

  changeSettingsType(e) {
    var settingsType = e.target.value
    this.props.onUpdate({tag: settingsType})
  }

  render() {
    var settings = this.props.settings || {}

    var form;
    if (settings.tag == Menu) {
      form = <MenuSettings settings={settings} onUpdate={this.props.onUpdate}/>
    }

    else {
      form = <TOCSettings settings={settings} onUpdate={this.props.onUpdate}/>
    }

    return <div>
      <label>Import Type</label>
      <select value={settings.tag} onChange={this.changeSettingsType.bind(this)}>
        <option value={TOC}>TOC</option>
        <option value={Menu}>Menu</option>
      </select>
      <div>{form}</div>
    </div>
  }
}

function emptyMenu() {
  return {
    tag: Menu
  }
}

function emptyTOC() {
  return {
    tocSelector: null,
  }
}

