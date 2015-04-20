
// @flow

import React from 'react'
import url from 'url'

export class Chapters extends React.Component {

  render() {
    var chapters = this.props.chapters || []
    var onUpdate = this.props.onUpdate
    //var source = this.props.source

    function row(chapter) {
      return <tr>
        <td>{chapter.chapterNumber}</td>
        <td>{chapter.chapterName}</td>
        <td><a href={chapter.chapterURL}>{urlPath(chapter.chapterURL)}</a></td>
      </tr>
    }

    return <div>
      <table>
        <tr>
          <th>Number</th>
          <th>Name</th>
          <th>URL</th>
        </tr>
        {chapters.map(row)}
      </table>
    </div>
  }
}

function urlPath(u) {
  var uri = url.parse(u)
  var out = uri.path
  if (uri.query)
    out += uri.query
  return out
}

//data Chapter = Chapter {
  //id :: Maybe Text,

  //sourceId :: Text,

  //chapterNumber :: Int,
  //chapterName :: Text,
  //chapterURL :: Text,
  //chapterHidden :: Bool

//} deriving (Show, Generic)

