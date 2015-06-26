// @flow

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
var {Menu, TOC, emptyImportSettings} = require('../model/source')
var {makeUpdate} = require('../data/update')


export class MenuSettings extends React.Component {

  render():?React.Element {
    var settings = this.props.settings

    var update = makeUpdate(settings, this.props.onUpdate)

    return <div>
      <label>Base URL</label>
      <input type="text" placeholder="http://fanfiction.net/s/11117811/" value={settings.menuBase} onChange={update((s, v) => s.menuBase = v)} />

      <div className="row">
        <div className="columns small-12">
          <label>Open Selector</label>
          <input placeholder="#chap_select" type="text" value={settings.menuOpen} onChange={update((s, v) => s.menuOpen = v)} />
        </div>
      </div>
    </div>
  }
}

export class TOCSettings extends React.Component {

  render():?React.Element {
    var settings = this.props.settings
    var update = makeUpdate(settings, (value) => {
      this.props.onUpdate(value)
    })

    return <div>
      <div className="row">
        <div className="columns small-12 medium-6">
          <label>Root Selector</label>
          <input placeholder="#toc" type="text"
            value={settings.tocSelector}
            onChange={update((s, v) => s.tocSelector = v)}
          />
        </div>

        <div className="columns small-12 medium-6">
          <label>Title Selector</label>
          <input placeholder="(leave blank for none)" type="text"
            value={settings.titleSelector}
            onChange={update((s, v) => s.titleSelector = v)}
          />
        </div>
      </div>
    </div>
  }
}


export class ImportSettings extends React.Component {

  changeSettingsType(e:any) {
    var settingsType = e.target.value
    var settings = emptyImportSettings(settingsType)
    this.props.onUpdate(settings)
  }

  render():?React.Element {
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
        <option value={TOC}>TOC - links on a page</option>
        <option value={Menu}>Menu - dropdown menu</option>
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

