import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { RecipesComponent } from './recipes.component';
import { ListComponent } from './list/list.component';
import { ItemComponent } from './item/item.component';
import { DetailComponent } from './detail/detail.component';

@NgModule({
  declarations: [
    RecipesComponent,
    ListComponent,
    ItemComponent,
    DetailComponent
  ],
  exports: [
    RecipesComponent
  ],
  imports: [
    CommonModule
  ]
})
export class RecipesModule { }
