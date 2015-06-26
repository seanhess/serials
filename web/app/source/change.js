// @flow

import React from 'react'
import {Link} from 'react-router'
import {Source} from '../model/source'
import {findChange, emptyChange, diffImport, diffChapter, changeUrl, FieldChange, diffChapters, ChapterDiff, emptyChapterDiff} from '../model/change'
import {Chapter, contentText, chapterContentURL, emptyChapter} from '../model/chapter'
import {FormSection} from '../comp'
import {diffBasic} from '../model/change'
import {toDateString} from '../helpers'
import {displayIf, Colors} from '../style'
import {assign} from 'lodash'
import {Routes} from '../router'


// render the source form
export class SourceChange extends React.Component {

  // load the change, then load the change it is based on

  static load(params) {
    var fc = findChange(params.id)

    return {
      change: fc,
      base: fc.then(c => findChange(c.baseId))
    }
  }

  render():React.Element {

    if (!this.props.change || !this.props.base) {
      return <div />
    }

    var change = this.props.change
    var base = this.props.base
    var user = change.createdBy

    var dBasic = diffBasic(base.source, change.source)
    var dImport = diffImport(base.source.importSettings, change.source.importSettings)
    var dChapters = diffChapters(base.source.chapters, change.source.chapters)

    var label = {
      fontWeight: 'bold'
    }

    var row = {
      marginBottom: 15
    }

    return <div>
      <h3>Change: <Link to={Routes.source} params={change.source}>{change.source.name}</Link></h3>
      <div className="row" style={{marginTop: 15}}>

        <div className="columns small-6">
          <ChangeHeader change={base}>Old</ChangeHeader>
          <ChangeInfo change={base}/>
        </div>

        <div className="columns small-6" style={{marginBottom: 15}}>
          <ChangeHeader change={change}>New</ChangeHeader>
          <ChangeInfo change={change}/>

          <ChangeArrow />
        </div>
      </div>

      <div style={{marginBottom: 25}}>
        <h5>Book Settings</h5>
        {dBasic.map(this.renderChange.bind(this))}
      </div>

      <div style={{marginBottom: 25}}>
        <h5>Scan Settings</h5>
        {dImport.map(this.renderChange.bind(this))}
      </div>

      <div style={{marginBottom: 25}}>
        <h5>Chapters</h5>
        {dChapters.map(this.renderChapterChange.bind(this))}
      </div>

    </div>
  }

  renderChange(change:FieldChange):React.Element {
    return <FieldChangeView key={change.field} change={change} />
  }

  renderChapterChange(change:FieldChange):React.Element {
    // it already works, but it doesn't show when they've been moved
    // if it has been deleted, show it

    var chapter:ChapterDiff = change.newValue || change.oldValue || emptyChapterDiff()
    var from:ChapterDiff = change.oldValue || emptyChapterDiff()
    var to:ChapterDiff = change.newValue || emptyChapterDiff()

    function title(d:ChapterDiff) {
      return <span>{'(' + d.index + ')'} {hidden(d)} {d.text}</span>
    }

    function hidden(d:ChapterDiff) {
      if (d.hidden) return <span className="fa fa-eye-slash"></span>
      return <span/>
    }

    return <div className="row" style={{marginBottom: 15}}>
      <div className="columns small-6">
        <ChangeBox title={title(from)}>
          <div>{from.url}</div>
        </ChangeBox>
      </div>

      <div className="columns small-6">
        <ChangeBox title={title(to)} change={change.change}>
          <div>{to.url}</div>
          <ChangeArrow />
        </ChangeBox>
      </div>
    </div>
  }
}

class ChangeArrow extends React.Component {

  render():React.Element {
    return <div style={{position: 'absolute', top: 10, left: -8, fontSize: 16}}>
      <span className="fa fa-long-arrow-right"></span>
    </div>
  }
}

var ChangeHeaderStyle = {
  borderBottom: 'solid 1px ' + Colors.dark,
  fontWeight: 'bold',
  padding: 10,
  paddingBottom: 0,
  paddingTop: 5,
  color: Colors.dark,
  background: Colors.light,
  marginBottom: 10
}

export class ChangeHeader extends React.Component {
  render():React.Element {
    var change = this.props.change
    return <div style={assign({}, ChangeHeaderStyle, this.props.style)}>
      <a className="right" href={changeUrl(change.id)} style={{marginTop: 5}}>
        <span className="fa fa-code"></span>
      </a>
      <h4>{this.props.children}</h4>
    </div>

  }
}

export class ChangeInfo extends React.Component {
  render():React.Element {
    var change = this.props.change
    return <div>
      <Link to={Routes.change} params={change}>
        <span>{toDateString(change.createdAt)} by {change.createdBy.firstName} {change.createdBy.lastName}</span>
      </Link>
    </div>
  }
}

export class FieldChangeView extends React.Component  {

  props: {
    change:FieldChange;
  };

  render():React.Element {
    var change = this.props.change || {}

    return <div style={{marginBottom: 0}}>
      <div className="row">
        <div className="columns small-6">
          <ChangeBox title={change.field}>
            {change.oldValue}
          </ChangeBox>
        </div>

        <div className="columns small-6">
          <ChangeBox title={change.field} change={change.change}>
            {change.newValue}
            <ChangeArrow />
          </ChangeBox>
        </div>
      </div>
    </div>
  }
}

class ChangeBox extends React.Component {

  render():React.Element {

    var change = this.props.change

    var fieldStyle = {
      fontWeight: 'bold',
      display: 'inline',
      marginRight: 10,
      fontSize: 12,
    }

    var valueStyle = {
      float: 'right'
    }

    var style = {
      fontSize: 12,
      background: Colors.code,
      border: 'solid 1px ' + Colors.codeTone,
      marginTop: 4,
      padding: 8,
      fontFamily: ['Consolas', "Liberation Mono", 'Menlo', 'Courier', 'monospace']
    }

    var newStyle = assign({}, style)

    if (change === "deleted") {
      newStyle.backgroundColor = Colors.diffRemove
    }

    else if (change === "new") {
      newStyle.backgroundColor = Colors.diffAdd
    }

    else if (change === "updated") {
      newStyle.backgroundColor = Colors.paper
    }

    else if (change === "unchanged") {
      newStyle.display = "none"
    }


    return <div style={newStyle}>
      <label style={fieldStyle}>{this.props.title}</label>
      <span style={valueStyle}>{this.props.children}</span>
      <div style={{clear: 'both'}}></div>
    </div>
  }
}
