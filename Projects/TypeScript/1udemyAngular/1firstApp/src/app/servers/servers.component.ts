import { Component } from '@angular/core';

@Component({
	selector: 'app-servers',     //  as an element, most commonly used
	// selector: '[app-servers]',    as an attribute
	// selector: '.app-servers', //  as a class
	templateUrl: './servers.component.html',
	// template: `
	//   <app-server></app-server>
	//   <app-server></app-server>`,
	styleUrls: ['./servers.component.css']
})
export class ServersComponent {
	allowNewServer: boolean = false;
	serverCreationStatus: string = 'No server was created!';
	serverName: string = '';
	serverCreated: boolean = false;
	servers: Array<string> = ['Testserver','Testserver 2'];

	// Assignment2
	username: string = '';

	checkUsername(): boolean {
		if (this.username === '') {
			return true
		}
		else {
			return false
		}
	}

	onResetUsername() {
		this.username = '';
	}
	//

	// Assignment3
	passwordDisplayed: boolean = false;
	logs: Array<number> = [];

	onDisplayPassword() {
		this.passwordDisplayed = true;
		this.logs.push(new Date().getTime()/1000)
	}
	//

	constructor() {
		setTimeout(
			() => {this.allowNewServer = true}, 
			2000);
	}

	onCreateServer() {
		this.serverCreationStatus = 'Server was created, with name: '+this.serverName;
		this.serverCreated = true;
		this.servers.push(this.serverName)
	}

	onUpdateServerName(event: Event ) {
		// console.log(event)
		this.serverName = (<HTMLInputElement>event.target).value;
	}
}
