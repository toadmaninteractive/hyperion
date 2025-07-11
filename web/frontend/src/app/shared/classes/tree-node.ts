export class TreeNode<T extends ITreeNode> {

    private _item: T;
    private _children: TreeNode<T>[];

    get children(): TreeNode<T>[] {
        return this._children;
    }

    set children(value: TreeNode<T>[]) {
        this._children = value;
    }

    get item(): T {
        return this._item;
    }

    set item(value: T) {
        this._item = value;
    }

}
