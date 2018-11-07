#include "openfilewindowclass.h"
#include "ui_openfilewindowclass.h"

OpenFileWindowClass::OpenFileWindowClass(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::OpenFileWindowClass)
{
    ui->setupUi(this);
}

OpenFileWindowClass::~OpenFileWindowClass()
{
    delete ui;
}
