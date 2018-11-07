#ifndef NEWFILEWINDOWCLASS_H
#define NEWFILEWINDOWCLASS_H

#include <QWidget>

namespace Ui {
class NewFileWindowClass;
}

class NewFileWindowClass : public QWidget
{
    Q_OBJECT

public:
    explicit NewFileWindowClass(QWidget *parent = nullptr);
    ~NewFileWindowClass();

private:
    Ui::NewFileWindowClass *ui;
};

#endif // NEWFILEWINDOWCLASS_H
