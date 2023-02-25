import { Component } from '@angular/core';

@Component({
  selector: 'app-warningalert',     //  as an element, most commonly used
  // selector: '[app-servers]',    as an attribute
  // selector: '.app-servers', //  as a class
  templateUrl: './warningalert.component.html',
  // template: `
  //   <app-server></app-server>
  //   <app-server></app-server>`,
  styles: [`
    body {
      color: white;
      padding: 20px;
      background-color: red;
      border: 1px solid red;
    }
  `]
})
export class WarningAlertComponent {

}
