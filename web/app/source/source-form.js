// @flow

import React from 'react'
import {Link} from 'react-router'

import {Source, SourceModel, emptySource, emptyScan, Status, defaultImageUrl, scan, validate, DefaultImageUrl, ScanResult} from '../model/source'
import {Chapter, emptyChapter, emptyTitle} from '../model/chapter'
import {findChanges, Change}  from '../model/change'
import {Alerts} from '../model/alert'
import {User, Users} from '../model/user'
import {toDateString} from '../helpers'
import {ChaptersList} from './chapters'
import {ImportSettings} from './import'
import {SourceChanges} from './changes'
import {DisabledButton, FormSection} from '../comp'

import {coverStyle, SourceCover} from '../cover'
import {makeUpdate, checked} from '../data/update'
import {displayIf} from '../style'
import {transitionTo, Routes} from '../router'

export class SourceEdit extends React.Component {

  props: {
    source: Source;
    changes: Array<Change>;
    params: {id: string};
  };

  state: {
    source: Source;
    scanning: boolean;
    changes: Array<Change>;
  };

  static load(params) {
    if (params.id == "new") {
      return {
        source: emptySource(DefaultImageUrl)
      }
    }

    return {
      source: SourceModel.find(params.id),
      changes: findChanges(params.id),
    }
  }

  constructor(props:any) {
    super(props)
    this.state = {source: emptySource(), scanning: false, changes:[]}
  }

  componentWillReceiveProps(props:any) {
    var source = props.source || emptySource()
    var chapters = source.chapters
    var changes = props.changes || []
    this.setState({source, chapters, changes})
  }

  onSaveClick() {

    var res = validate(this.state.source)

    if (res) {
      Alerts.update("error", res)
      return
    }

    if (this.isNew()) {
      return this.create()
    }

    else {
      return this.save()
    }
  }

  onDelete() {
    return SourceModel.delete(this.props.params.id)
    .then(function() {
      Alerts.update("success", "Source has been deleted", true)
      transitionTo(Routes.library)
    })
  }

  isNew():boolean {
    return this.props.params.id == "new"
  }

  updateChapters(chapters:Array<Chapter>) {
    var source:Source = this.state.source
    source.chapters = chapters;
    this.setState({source: source})
  }

  // need to reload the change...
  save() {
    var source = this.state.source
    return SourceModel.save(this.props.source.id, source)
    .then(() => {
      Alerts.update("success", "Saved!", true)
      transitionTo(Routes.book, this.state.source)
    })
  }

  create() {
    var source = this.state.source
    return SourceModel.create(source)
    .then((id) => {
      Alerts.update("success", "Created!", true)
      transitionTo(Routes.book, {id})
    })
  }

  runScan() {
    this.setState({scanning: true})
    scan(this.state.source)
    .then((res:ScanResult) => {
      var source:Source = this.state.source
      source.chapters = res.allChapters
      source.lastScan = res.scan
      this.setState({source: source})
    })
    .then(() => this.setState({scanning: false}))
    .then(() => Alerts.update("success", "Scan complete"))
  }

  deleteAllChapters() {
    var source = this.state.source
    source.chapters = []
    this.setState({source: source})
  }

  addNewLink() {
    var chapter = emptyChapter()
    this.updateChapters(this.state.source.chapters.concat([chapter]))
  }

  addNewTitle() {
    var chapter = emptyChapter(emptyTitle())
    this.updateChapters(this.state.source.chapters.concat([chapter]))
  }

  updateSource(source:Source) {
    this.setState({source})
  }

  render():?React.Element {
    var source:Source = this.state.source || {}
    var chapters = source.chapters || []
    var lastScan = source.lastScan || emptyScan()

    var scanningDisabled = (this.state.scanning) ? "disabled" : ""
    var scanningText = (this.state.scanning) ? "Scanning..." : "Scan Now"

    var update = makeUpdate(source, this.updateSource.bind(this))

    var cancelRoute = (this.isNew()) ? Routes.library : Routes.book
    var title = (this.isNew()) ? "New Book" : "Edit Book"

    return <div className="row">
      <div className="columns small-12">
        <h3>{title}</h3>

        <div>
          <button className="" onClick={this.onSaveClick.bind(this)}>Save Changes</button>
          <span> </span>

          <Link to={cancelRoute} params={{id: source.id}} className="button secondary">Cancel</Link>
          <span> </span>
          <div className="right" style={displayIf(Users.isAdmin())}>
            <button className="secondary" onClick={this.onDelete.bind(this)}>Delete</button>
          </div>
        </div>

        <BookDetails source={source} update={update} />
        <ImageDetails source={source} update={update} />
        <ScanSettings source={source} update={this.updateSource.bind(this)} />

        <hr />

        <div>
          <h4>{chapters.length} Chapters</h4>

          <p>
            Last Scan: {toDateString(lastScan.date)}
          </p>

          <div className="right">
            <button className="secondary" onClick={this.addNewTitle.bind(this)}>Add Title</button>
            <span> </span>
            <button className="secondary" onClick={this.addNewLink.bind(this)}>Add Chapter</button>
          </div>

          <div className="">
            <button className={scanningDisabled} onClick={this.runScan.bind(this)}>{scanningText}</button>
            <span> </span>
            <button className="secondary" onClick={this.deleteAllChapters.bind(this)}>Delete All</button>
          </div>

          <ChaptersList chapters={chapters} source={source}
            update={this.updateChapters.bind(this)}
          />
        </div>

        <div style={displayIf(this.state.changes.length > 0)}>
          <hr />
          <h4>Changes</h4>
          <SourceChanges changes={this.state.changes}/>
        </div>

      </div>
    </div>

  }
}

export class BookDetails extends React.Component {

  props: {
    source: Source;
    update: Function;
    disabled: boolean;
  };

  render():React.Element {
    var source = this.props.source
    var update = this.props.update

    return <FormSection title="Book Details">
      <label>Title</label>
      <input type="text"
        value={source.name}
        onChange={update((s, v) => s.name = v)}
      />

      <div className="row">
        <div className="columns small-12 medium-6">
          <label>Author</label>
          <input type="text"
            value={source.author}
            onChange={update((s, v) => s.author = v)}
          />
        </div>

        <div className="columns small-12 medium-6">
          <label>Author URL</label>
          <input type="text"
            value={source.authorUrl}
            onChange={update((s, v) => s.authorUrl = v)}
          />
        </div>
      </div>

      <div className="row" style={displayIf(Users.isAdmin())}>
        <div className="columns small-12 medium-3">
          <label>Status</label>
          <StatusSelect update={update} source={source} />
        </div>

        <div className="columns small-12 medium-3">
          <label>Hidden</label>
          <input type="checkbox"
            checked={source.hidden}
            onChange={update((s, v) => s.hidden = v, checked)}
          />
        </div>
        <div className="columns medium-9"></div>
      </div>

    </FormSection>
  }
}


class ImageDetails extends React.Component {

  props: {
    source: Source;
    update: Function
  };

  render():React.Element {
    var source = this.props.source
    var update = this.props.update

    return <FormSection title="Image Details">

      <div>
        <div style={{float: 'left', width: 170}}>
          <SourceCover source={source} />
        </div>

        <div style={{marginLeft: 170, minHeight: 250}}>
          <label>Image URL</label>
          <input type="text"
            value={source.imageUrl}
            onChange={update(function(s, v) {
              s.imageUrl = defaultImageUrl(v)
            })}
          />

          <input type="checkbox"
            checked={source.imageMissingTitle}
            onChange={update((s, v) => s.imageMissingTitle = v, checked)}
          />
          <label>Image Missing Title</label>

          <label>Artist</label>
          <input type="text"
            value={source.imageArtist}
            onChange={update((s, v) => s.imageArtist = v)}
          />

          <label>Artist URL</label>
          <input type="text"
            value={source.imageArtistUrl}
            onChange={update((s, v) => s.imageArtistUrl = v)}
          />

          <label>Artist About URL</label>
          <input type="text"
            value={source.imageArtistAboutUrl}
            onChange={update((s, v) => s.imageArtistAboutUrl = v)}
          />

        </div>
      </div>

    </FormSection>
  }
}

class ScanSettings extends React.Component {

  props: {
    source: Source;
    update: Function
  };

  updateSettings(value) {
    var source = this.props.source
    source.importSettings = value
    this.props.update(source)
  }

  render():React.Element {
    var source = this.props.source
    var update = this.props.update

    var update = makeUpdate(source, (v) => {
      this.props.update(v)
    })

    return <FormSection title="Scan Settings">
      <label>Table of Contents URL</label>
      <input type="text"
        placeholder="https://example.com/table-of-contents/"
        value={source.url}
        onChange={update((s, v) => s.url = v)}
      />

      <ImportSettings
        settings={source.importSettings}
        onUpdate={this.updateSettings.bind(this)}
      />

    </FormSection>
  }
}

class StatusSelect extends React.Component {

  render():React.Element {
    var source = this.props.source
    var update = this.props.update
    var options = Status.All.map(s => <option value={s} key={s}>{s}</option>)

    return <select
        value={source.status}
        onChange={update((s, v) => s.status = v)}>
        {options}
      </select>
  }
}




