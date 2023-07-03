import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { EditComponent } from './edit/edit.component';
import { ShoppingComponent } from './shopping.component';
import { ListComponent } from './list/list.component';



@NgModule({
  declarations: [
    EditComponent,
    ShoppingComponent,
    ListComponent
  ],
  exports: [
    ShoppingComponent
  ],
  imports: [
    CommonModule
  ]
})
export class ShoppingModule { }
