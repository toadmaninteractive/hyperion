import { ChangeDetectorRef, Component, Inject, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';
import { BehaviorSubject, Subject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
import { TreeComponent } from '../../../../components/general/tree/tree.component';
import * as DataProtocol from '../../../../protocol/data-protocol.data';
import * as DbProtocol from '../../../../protocol/db-protocol.data';
import { ParameterService } from '../../../../core/services/parameter.service';

export interface IBrowseParamData {
    project: DbProtocol.Project;
    paramData: DataProtocol.TreeNode<DbProtocol.Parameter>[] | null;
    setupNode: DataProtocol.TreeNode<DbProtocol.SetupStep>;
}

@Component({
    selector: 'm-browse-params-dialog',
    templateUrl: 'browse-params-dialog.component.html',
    styleUrls: ['./browse-params-dialog.component.scss'],
})

export class BrowseParamsDialogComponent implements OnInit, OnDestroy {
    @ViewChild(TreeComponent) tree: TreeComponent;
    destroy$: Subject<any>;
    loading$ = new BehaviorSubject<boolean>(true);

    hideUnsetParameters = false;
    hiddenNodeSetId = new Set<number>();
    unsetParameters = new Set<number>();

    constructor(
        public dialogRef: MatDialogRef<BrowseParamsDialogComponent>,
        @Inject(MAT_DIALOG_DATA) public data: IBrowseParamData,
        private cdr: ChangeDetectorRef,
        public parametersService: ParameterService,
    ) {
    }

    ngOnInit(): void {
        this.destroy$ = new Subject();
        this.parametersService.parameterList$
            .asObservable()
            .pipe(takeUntil(this.destroy$))
            .subscribe(paramList => {
                paramList.forEach(param => {
                    if (param.item.values.length === 0) {
                        this.unsetParameters.add(param.item.id);
                    }});
            });
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
        this.loading$.complete();
    }

    selectParameter(node: DataProtocol.TreeNode<DbProtocol.Parameter>) {
        this.dialogRef.close({ paramNode: node, setup: this.data.setupNode });
    }

    checkFilter() {
        this.hideUnsetParameters = !this.hideUnsetParameters;
        if (this.hideUnsetParameters) {
            this.hiddenNodeSetId = new Set(this.unsetParameters);
        } else {
            this.hiddenNodeSetId = new Set();
        }
        this.cdr.detectChanges();
    }
}
