#ifndef SPRITECONTROLLER_H
#define SPRITECONTROLLER_H

#include <QWidget>

namespace Ui {
class SpriteController;
}

class SpriteController : public QWidget
{
    Q_OBJECT

public:
    explicit SpriteController(QWidget *parent = nullptr);
    ~SpriteController();

private:
    Ui::SpriteController *ui;
};

#endif // SPRITECONTROLLER_H
