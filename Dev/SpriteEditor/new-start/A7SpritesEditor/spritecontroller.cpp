#include "spritecontroller.h"
#include "ui_spritecontroller.h"

SpriteController::SpriteController(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::SpriteController)
{
    ui->setupUi(this);
}

SpriteController::~SpriteController()
{
    delete ui;
}
