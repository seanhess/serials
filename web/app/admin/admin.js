
// @flow

import React from 'react'
import {Link} from 'react-router'

import {RouteHandler} from 'react-router'
import {MainContainer, Header} from '../layout/main'
import {importLog} from '../model/admin'
import {settings} from '../model/settings'
import {FormSection} from '../comp'

export class Admin extends React.Component {
  render():React.Element {
    return <div>
      <Header {...this.props}/>
      <MainContainer alert={this.props.alert}>
        <RouteHandler {...this.props}/>
      </MainContainer>
    </div>
  }
}

export class ImportLog extends React.Component {

  static load(params) {
    return {text: importLog(params.n)}
  }

  render():React.Element {
    var numLines = parseInt(this.props.params && this.props.params.n, 10)
    var moreUrl = "#/admin/import-log/" + (numLines+500)

    return <div>
      <h2>Import Log</h2>
      <div><pre style={{fontSize: '14px'}}>{this.props.text}</pre></div>
      <a href={moreUrl}>More</a>
    </div>
  }
}

export class AdminDashboard extends React.Component {

  render():React.Element {
    var sets = settings()
    return <div>
      <div>
        <h3>Admin</h3>

        <p className="row">
          <label className="columns small-12 medium-2">Endpoint</label>
          <code className="columns small-12 medium-10" style={{fontSize: 'smaller'}}>{sets.appEndpoint}</code>
        </p>

        <p className="row">
          <label className="columns small-12 medium-2">Version</label>
          <code className="columns small-12 medium-10" style={{fontSize: 'smaller'}}>{sets.version}</code>
        </p>

        <hr />

        <div>
          <FormSection title="Sources">
            <div>
              <Link className="button info" to="sources">
                <span className="fa fa-book"></span>
                <span> Sources</span>
              </Link>
            </div>
            <div><a href="#/admin/import-log/500">Import Log</a></div>
          </FormSection>

          <FormSection title="Users">
            <Link className="button info" to="admin-users">
              <span className="fa fa-user"></span>
              <span> Users</span>
            </Link>
            <span> </span>
            <Link className="button info" to="invites">
              <span className="fa fa-paper-plane"></span>
              <span> Invites</span>
            </Link>
          </FormSection>

        </div>

      </div>
    </div>
  }
}
