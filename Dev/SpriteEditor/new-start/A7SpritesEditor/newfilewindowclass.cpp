#include "newfilewindowclass.h"
#include "ui_newfilewindowclass.h"

NewFileWindowClass::NewFileWindowClass(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::NewFileWindowClass)
{
    ui->setupUi(this);
}

NewFileWindowClass::~NewFileWindowClass()
{
    delete ui;
}
