// @flow

import React from 'react'
import {Link} from 'react-router'

import {Source, SourceModel, emptySource, emptyScan, Status, defaultImageUrl, Change, findChanges} from '../model/source'
import {ChapterModel, Chapter, emptyChapter, emptyTitle} from '../model/chapter'
import {Alerts} from '../model/alert'
import {toDateString} from '../helpers'
import {Chapters} from './chapters'
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
    chapters: Array<Chapter>;
    changes: Array<Change>;
    params: {id: string};
  };

  static load(params) {
    if (params.id == "new") {
      return {source: emptySource(), chapters:[]}
    }

    return {
      source: SourceModel.find(params.id),
      chapters: ChapterModel.findBySource(params.id),
      changes: findChanges(params.id),
    }
  }

  constructor(props:any) {
    super(props)
    this.state = {source: emptySource(), scanning: false, chapters: []}
  }

  componentWillReceiveProps(props:any) {
    this.setState({
      // store them locally so you can refresh them!
      source: props.source || emptySource(),
      chapters: props.chapters || []
    })
  }


  onSaveClick() {
    if (this.isNew()) {
      return this.create()
    }

    else {
      return this.save()
    }
  }

  isNew():boolean {
    return this.props.params.id == "new"
  }

  onSaveChapter(chapter:Chapter) {
    ChapterModel.save(chapter)
    .then(this.reloadChapters.bind(this))
  }

  reloadChapters() {
    return ChapterModel.findBySource(this.props.params.id)
    .then((chapters) => {
      this.setState({chapters: chapters})
    })
  }

  onClearChapter(chapter:Chapter) {
    ChapterModel.clear(chapter)
    .then(this.reloadChapters.bind(this))
  }

  onDeleteChapter(chapter:Chapter) {
    ChapterModel.delete(chapter.id)
    .then(this.reloadChapters.bind(this))
  }

  onHiddenChapter(chapter:Chapter, hidden:boolean) {
    ChapterModel.hidden(chapter, hidden)
    .then(this.reloadChapters.bind(this))
  }

  save() {
    var source = this.state.source
    return SourceModel.save(this.props.source.id, source)
    .then(() => Alerts.update("success", "Saved!"))
  }

  create() {
    var source = this.state.source
    return SourceModel.create(source)
    .then(() => transitionTo(Routes.sources))
  }

  runScan() {
    if (this.isNew()) {
      throw new Error("Cannot scan new source")
    }

    // save first
    this.save()
    .then(() => this.setState({scanning: true}))
    .then(() => ChapterModel.importSource(this.props.params.id))
    .then(() => this.setState({scanning: false}))
    .then(this.reloadChapters.bind(this))
    .then(() => Alerts.update("success", "Scan complete"))
  }

  deleteAllChapters() {
    ChapterModel.deleteBySource(this.props.params.id)
    .then(this.reloadChapters.bind(this))
  }

  onUpdateSettings(settings:ImportSettings) {
    var source = this.state.source
    source.importSettings = settings
    this.setState({source: source})
  }

  addNewLink() {
    var chapter = emptyChapter(this.state.source.id)
    ChapterModel.save(chapter).then(this.reloadChapters.bind(this))
  }

  addNewTitle() {
    var chapter = emptyChapter(this.state.source.id, emptyTitle())
    ChapterModel.save(chapter).then(this.reloadChapters.bind(this))
  }

  render():?React.Element {
    var source:Source = this.state.source || {}
    var chapters = this.state.chapters || []
    var lastScan = source.lastScan || emptyScan()

    var scanningDisabled = (this.state.scanning) ? "disabled" : ""
    var scanningText = (this.state.scanning) ? "Scanning..." : "Scan Now"

    var update = makeUpdate(source, (v) => {
      this.setState({source: v})
    })

    return <div>
      <h3>Source</h3>

      <div>
        <button className="" onClick={this.onSaveClick.bind(this)}>Save</button>
        <span> </span>
        <div className="right">
          <Link to={Routes.book} params={{id: source.id}}>View Book</Link>
        </div>
      </div>

      <BookDetails source={source} update={update} />
      <ImageDetails source={source} update={update} />
      <ScanSettings source={source} update={update} />

      <hr />

      <div style={displayIf(!this.isNew())}>
        <h4>{chapters.length} Chapters</h4>

        <div>
          Last Scan
          <ul>
            <li>Date: {toDateString(lastScan.date)}</li>
            <li>Total: {lastScan.total}</li>
            <li>New: {lastScan.new.length}</li>
            <li>Updated: {lastScan.updated.length}</li>
          </ul>
        </div>

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

        <Chapters chapters={chapters} source={source}
          onSaveChapter={this.onSaveChapter.bind(this)}
          onClearChapter={this.onClearChapter.bind(this)}
          onDeleteChapter={this.onDeleteChapter.bind(this)}
          onHiddenChange={this.onHiddenChapter.bind(this)}
        />
      </div>

      <hr />

      <h4>Changes</h4>
      <SourceChanges changes={this.props.changes}/>

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

      <div className="row">
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

  render():React.Element {
    var source = this.props.source
    var update = this.props.update

    return <FormSection title="Scan Settings">
      <label>Table of Contents URL</label>
      <input type="text"
        placeholder="https://example.com/table-of-contents/"
        value={source.url}
        onChange={update((s, v) => s.url = v)}
      />

      <ImportSettings
        settings={source.importSettings}
        onUpdate={update((s, v) => s.importSettings = v)}
      />

    </FormSection>
  }
}

class StatusSelect extends React.Component {

  render():React.Element {
    var source = this.props.source
    var update = this.props.update
    var options = Status.All.map(s => <option value={s}>{s}</option>)

    return <select
        value={source.status}
        onChange={update((s, v) => s.status = v)}>
        {options}
      </select>
  }
}




